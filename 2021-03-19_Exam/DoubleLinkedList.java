import java.util.function.Consumer;

// Not performant at all
class DoubleLinkedList<E> {

    // Inner class.
    // Synchronized all of the variables that will ever get obtained, these are the ones
    // that will always be interacted with so I placed them into synchronized blocks.
    class Item<E> {
        // Inner class instance variables.
        private E content;
        private Item<E> prev;
        private Item<E> next;

        // Inner class constructor.
        Item(E element) {
            content = element;
            prev = null;
            next = null;
        }

        synchronized Item<E> getPrev() {
            return prev;
        }

        synchronized Item<E> getNext() {
            return next;
        }

        synchronized void setNext(Item<E> next) {
            this.next = next; 
        }

        synchronized void setPrev(Item<E> prev) {
            this.prev = prev;
        }

        synchronized E getContent() {
            return content;
        }
    }

    // Instance variables.
    private final Item<E> first;
    private final Item<E> last;

    // Constructor.
    DoubleLinkedList() {
        // We include two dummy items (one first and one last) to simplify the code.
        first = new Item<E>(null);
        last = new Item<E>(null);
        first.setNext(last);
        last.setPrev(first);
    }

    // Instance methods.

    // Inserts the specified element at the specified position in this list.
    // Synchronize entire interaction to ensure thread safety
    void add(int index, E element) {
        synchronized (this) {
            Item<E> item = new Item<E>(element);
            Item<E> temp = first;

            // Find the item just before where the new item should be inserted.
            for (int i = 0; i < index; i++) {
                temp = temp.getNext();
                if (temp.getNext() == null)
                    throw new IndexOutOfBoundsException();
            }
            // Insert the new item.
            temp.getNext().setPrev(item);
            item.setNext(temp.next);
            temp.setNext(item);
            item.setPrev(temp);
        }
    }

    // Removes and returns the element at the specified position in this list.
    // Synchronize entire interaction to ensure thread safety
    E remove(int index) {
        synchronized (this) {
            Item<E> item = null;
            Item<E> temp = first;

            // Find the item just before the item that should be removed.
            if (temp.getNext().getNext() == null)
                throw new IndexOutOfBoundsException();
            for (int i = 0; i < index; i++) {
                temp = temp.getNext();
                if (temp.getNext().getNext() == null)
                    throw new IndexOutOfBoundsException();
            }

            // Remove the item.
            item = temp.getNext();
            temp.getNext().getNext().setPrev(temp);
            temp.setNext(temp.getNext().getNext());
            item.setPrev(null);
            item.setNext(null);

            return item.getContent();
        }
    }

    // Performs the given action for each element in this list.
    // Synchronize entire interaction to ensure thread safety
    void forEach(Consumer<E> action) {
        synchronized (this) {
            Item<E> temp = first;

            while (temp.getNext().getNext() != null) {
                temp = temp.getNext();
                action.accept(temp.getContent());
            }
        }

    }

    // Execution entry point.
    public static void main(String[] args) {
        // Here you can write some test program.
    }
}
