module SLL.Descriptions exposing (..)


get : String
get =
    """
    private Node<T> nodeAt(int index) {  

        Node<T> current = this.first;
        for (int i = 0; i < index; i++) {
            current = current.next;
        }
        return current;
    }

    public T get(int index) {
        return nodeAt(index).value;
    }
    """
