package org.jboss.tools.runtime.ui.download;

import java.util.HashMap;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeFilter;
import org.jboss.tools.runtime.core.model.IDownloadRuntimes;

public class DownloadRuntimes implements IDownloadRuntimes {
	public static final String SHELL = IDownloadRuntimes.SHELL;
	
	public DownloadRuntimes() {
		
	}
	public void execute(HashMap<String, Object> map) {
		Object shell = map.get(SHELL);
		Shell shell2 = shell == null ? Display.getDefault().getActiveShell() : ((Shell)shell);
		Object filter = map.get(IDownloadRuntimes.RUNTIME_FILTER);
		
		// If this has not been accessed before, this may freeze the UI during 
		// a fetch to the remote path. The call to get the downloadable runtimes
		// also fetches from a remote repository location. 
		// THis should also be done via a display.asynchexec
		DownloadRuntimeViewerDialog dialog = new DownloadRuntimeViewerDialog(shell2, (IDownloadRuntimeFilter)filter);
		dialog.open();
	}

}
