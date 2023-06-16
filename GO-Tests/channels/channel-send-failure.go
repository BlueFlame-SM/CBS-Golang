package main;

func main() {
  channel := make(chan any);
  channel <- 1;
};
