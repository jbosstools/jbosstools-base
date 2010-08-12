package org.jboss.tools.ui.bot.ext;

import static org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable.syncExec;

import org.apache.log4j.Logger;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swtbot.swt.finder.results.Result;
import org.eclipse.ui.IWorkbenchWindow;

public class SWTDebugExt {

	SWTBotExt bot = new SWTBotExt();
	Logger log = Logger.getLogger(SWTDebugExt.class);
	
	/**
	 * Print SWT data in hierarchy
	 */
	public void swtInfo(Composite composite) {
			final Composite c = composite; 
			syncExec(new Result<IWorkbenchWindow>() {

				public IWorkbenchWindow run() {
					printRecursive(c);
					return null;
				}
			});
		}
	
	private void printRecursive(Composite composite) {
		if (composite.getData() != null) {
			
			composite.toString();
		}
	}
}
