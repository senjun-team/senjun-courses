import socket
from select import select

PORT = 1234
HOST = "localhost"


class Server:
    def __init__(self) -> None:
        self._tasks = []
        self._sockets_to_read_with_continue = {}
        self._socket_to_write_with_continue = {}

    def start(self, host, port):
        self._tasks.append(self._start_server(host, port))
        self._event_loop()

    @staticmethod
    def _process_client(client_socket) -> None:
        while True:
            yield ("read", client_socket)
            request = client_socket.recv(4096)

            if not request:
                break
            else:
                response = f"Hello, world! {request} \n".encode()
                yield ("write", client_socket)
                client_socket.send(response)

        client_socket.close()

    def _start_server(self, host, port) -> None:
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind((host, port))
        server_socket.listen()

        while True:
            yield ("read", server_socket)
            client_socket, addr = server_socket.accept()

            print(f"Connection from {addr}")
            self._tasks.append(self._process_client(client_socket))

    def _event_loop(self) -> None:
        while any(
            [
                self._tasks,
                self._sockets_to_read_with_continue,
                self._socket_to_write_with_continue,
            ]
        ):
            while not self._tasks:
                sockets_ready_to_read, sockets_ready_to_write, _ = select(
                    self._sockets_to_read_with_continue,
                    self._socket_to_write_with_continue,
                    [],
                )

                for sock in sockets_ready_to_read:
                    self._tasks.append(self._sockets_to_read_with_continue.pop(sock))

                for sock in sockets_ready_to_write:
                    self._tasks.append(self._socket_to_write_with_continue.pop(sock))

            try:
                task = self._tasks.pop(0)

                reason, sock = next(task)
                if reason == "read":
                    self._sockets_to_read_with_continue[sock] = task

                if reason == "write":
                    self._socket_to_write_with_continue[sock] = task
            except StopIteration:
                print("Connection closed!")  # here we exited from generator


def main():
    s = Server()
    s.start(HOST, PORT)


if __name__ == "__main__":
    main()
