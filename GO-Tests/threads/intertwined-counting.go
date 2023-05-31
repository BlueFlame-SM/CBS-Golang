package main;

func main() {
  go thread();
  for i := 5; i < 10; i++ {
    print(i);
  };
};

func thread() {
  for i := 0; i < 5; i++ {
    print(i);
  };
};
