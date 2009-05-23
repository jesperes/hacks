package se.eskilson.buildengine.filemonitor;

import java.io.File;
import java.util.Collection;

public interface IMonitoredFile {

	public void rescan(Collection<IFileListener> listeners);

	void notifyListeners(Collection<IFileListener> listeners, File f,
			FileEvent event);

}
