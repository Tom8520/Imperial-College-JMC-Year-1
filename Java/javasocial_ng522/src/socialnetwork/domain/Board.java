package socialnetwork.domain;

import java.util.List;

public interface Board {

  boolean addMessage(Message message);

  boolean deleteMessage(Message message);

  int size();

  List<Message> getBoardSnapshot();
}
