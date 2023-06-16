package main;

func main() {
  c := make(chan int, 10);
  c <- 13;
  print(<-c);
};
