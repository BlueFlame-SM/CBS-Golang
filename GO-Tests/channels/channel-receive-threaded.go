package main;

func main() {
  c := make(chan int, 10);
  go sender(c);
  print(<-c);
};

func sender(c any) {
  c <- 1;
};
