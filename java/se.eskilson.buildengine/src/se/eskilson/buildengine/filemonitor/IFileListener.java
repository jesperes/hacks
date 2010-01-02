package se.eskilson.buildengine.filemonitor;

import java.io.File;

public interface IFileListener {
	public void fileChanged(File f, FileEvent event);
}
