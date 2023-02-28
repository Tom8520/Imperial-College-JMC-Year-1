package socialnetwork.domain;

import java.util.List;
import socialnetwork.collections.Vector;

public class UserBoard implements Board {

  Vector<Message> messages = new Vector<>();

  @Override
  synchronized public boolean addMessage(Message message) {
    return messages.add(message);
  }

  @Override
  synchronized public boolean deleteMessage(Message message) {
    return messages.remove(message);
  }

  @Override
  synchronized public int size() {
    return messages.size();
  }

  @Override
  synchronized public List<Message> getBoardSnapshot() {
    List<Message> messageList = messages.toList();
    messageList.sort(null);
    return messageList;
  }
}
