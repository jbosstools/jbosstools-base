package org.jboss.tools.runtime.ui.download;

import java.util.HashMap;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.runtime.ui.IDownloadRuntimes;

public class DownloadRuntimes implements IDownloadRuntimes {
	public static final String SHELL = "download.runtimes.shell";
	@Override
	public void execute(HashMap<String, Object> map) {
		Object shell = map.get(SHELL);
		Shell shell2 = shell == null ? Display.getDefault().getActiveShell() : ((Shell)shell);
		DownloadRuntimeViewerDialog dialog = new DownloadRuntimeViewerDialog(shell2);
		dialog.open();
	}

}
