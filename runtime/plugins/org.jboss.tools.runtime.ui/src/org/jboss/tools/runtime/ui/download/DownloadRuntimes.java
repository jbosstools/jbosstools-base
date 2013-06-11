package org.jboss.tools.runtime.ui.download;

import java.util.HashMap;

import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeFilter;
import org.jboss.tools.runtime.core.model.IDownloadRuntimes;
import org.jboss.tools.runtime.ui.internal.wizard.DownloadRuntimesWizard;

public class DownloadRuntimes implements IDownloadRuntimes {
	public static final String SHELL = IDownloadRuntimes.SHELL;
	public static final String DOWNLOAD_LAUNCHED = IDownloadRuntimes.DOWNLOAD_LAUNCHED;
	
	public DownloadRuntimes() {
		
	}
	public void execute(HashMap<String, Object> map) {
		Object shell = map.get(SHELL);
		Shell shell2 = shell == null ? Display.getDefault().getActiveShell() : ((Shell)shell);
		IDownloadRuntimeFilter filter = (IDownloadRuntimeFilter)map.get(IDownloadRuntimes.RUNTIME_FILTER);
		WizardDialog dialog = new WizardDialog(shell2, new DownloadRuntimesWizard(shell2, filter));
		dialog.open();
	}
}
