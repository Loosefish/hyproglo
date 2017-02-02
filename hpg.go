package main
import (
    "encoding/json"
    "fmt"
    "io/ioutil"
    "log"
    "net"
    "net/http"
    "os"
    "path"
    "strings"
)


var socketAddress string
var mpdRoot *string


type mpdReply struct {
    Status string `json:"status"`
    Result string `json:"result"`
    Error error `json:"error"`
}


func mpd(q string, addr string) (string, error) {
    if !strings.HasSuffix(q, "\n") {
        q += "\n"
    }

    conn, err := net.Dial("unix", addr)
    if err != nil {
        return "", err
    }

    var buf = make([]byte, 4 * 1024)
    read, err := conn.Read(buf)
    if read < 1 || err != nil {
        conn.Close()
        return "", err
    }

    status := string(buf[:read])
    if !strings.HasPrefix(status, "OK MPD") {
        conn.Close()
        return "", fmt.Errorf("Unexcpected MPD status: %s", status)
    }

    conn.Write([]byte(q))
    read, err = conn.Read(buf)
    if err != nil {
        return "", err
    }

    reply := string(buf[:read])
    if strings.HasPrefix(reply, "ACK") {
        return "", fmt.Errorf("MPD error: %s", reply)
    }

    for !strings.HasSuffix(reply, "OK\n") {
        read, err = conn.Read(buf)
        if err != nil {
            return "", err
        }
        reply += string(buf[:read])
    }
    conn.Close()
    return reply, nil
}


func mpdHandler(w http.ResponseWriter, r *http.Request) {
    if r.Method != "POST" {
        http.Error(w, "Unsupported method, use POST.", 405)
        return
    }

    data, err := ioutil.ReadAll(r.Body)
    if err != nil {
        http.Error(w, err.Error(), 400)
        return
    }

    userQuery := string(data)
    reply, err := mpd(userQuery, socketAddress)
    js, err := json.Marshal(mpdReply{"", reply, err})
    if err != nil {
        http.Error(w, err.Error(), 500)
        return
    }
    fmt.Fprint(w, string(js))
}


func imageHandler(w http.ResponseWriter, r *http.Request) {
    if mpdRoot == nil {
        res, err := mpd("config", socketAddress)
        if err != nil {
            http.Error(w, err.Error(), 503)
            return
        }
        root := strings.TrimPrefix(res, "music_directory: ")
        root = strings.TrimSuffix(root, "\nOK\n")
        mpdRoot = &root
    }
    img_path := strings.TrimPrefix(r.URL.Path, "/image/")
    img_path = path.Dir(path.Join(*mpdRoot, img_path))

    files, err := ioutil.ReadDir(img_path)
    if err != nil {
        http.Error(w, err.Error(), 404)
        return
    }

    for _, file := range files {
        img_file := file.Name()
        ext := strings.ToLower(path.Ext(img_file))
        if ext == ".jpg" || ext == ".jpeg" || ext == ".png" || ext == ".gif" {
            http.ServeFile(w, r, path.Join(img_path, img_file))
            return
        }
    }
}


func main() {
    if len(os.Args) != 3 {
        fmt.Printf("Usage: %s PORT MPD_SOCKET\n", os.Args[0])
        os.Exit(1)
    }
    port := os.Args[1]
    socketAddress = os.Args[2]

    fs := http.FileServer(http.Dir("server"))
    http.Handle("/", fs)
    http.HandleFunc("/mpd", mpdHandler)
    http.HandleFunc("/image/", imageHandler)

    log.Fatal(http.ListenAndServe(":" + port, nil))
}
