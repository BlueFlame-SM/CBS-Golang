package main;

var i = 0;

func main() {
  c := make(chan int, 20);
  for j := 0; j < 3; j++ {
    go sender(c, j);
  };
  for j := 0; j < 20; j++ {
    println(<-c);
  };
};

func sender(c chan int, j int) {
  for ; i < 20; i++ {
    c <- j;
  };
};
