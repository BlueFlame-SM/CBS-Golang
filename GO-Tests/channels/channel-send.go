package main;

func main() {
  channel := make(chan any, 1);
  channel <- 1;
};
