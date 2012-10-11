package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.jboss.reddeer.eclipse.jface.preference.PreferencePage;
import org.jboss.reddeer.swt.api.Table;
import org.jboss.reddeer.swt.condition.JobIsRunning;
import org.jboss.reddeer.swt.condition.WaitCondition;
import org.jboss.reddeer.swt.exception.WidgetNotAvailableException;
import org.jboss.reddeer.swt.impl.button.PushButton;
import org.jboss.reddeer.swt.impl.label.DefaultLabel;
import org.jboss.reddeer.swt.impl.shell.DefaultShell;
import org.jboss.reddeer.swt.impl.table.DefaultTable;
import org.jboss.reddeer.swt.wait.TimePeriod;
import org.jboss.reddeer.swt.wait.WaitUntil;
import org.jboss.reddeer.swt.wait.WaitWhile;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

public class RuntimeDetectionPreferencesDialog extends PreferencePage {

	public RuntimeDetectionPreferencesDialog() {
		super("JBoss Tools", "JBoss Tools Runtime Detection");
	}
	
	@Override
	public void ok(){
		new WaitWhile(new JobIsRunning());
		super.ok();
	}
	
	public SearchingForRuntimesDialog addPath(final String path){
		RuntimeUIActivator.getDefault().getModel().addRuntimePath(new RuntimePath(path));
		
		// ↓ MacOSX cannot open preferences dialog for the first time - trying to resolve
		new WaitWhile(new JobIsRunning());
		cancel();
		new WaitWhile(new JobIsRunning());
		try{
			open();
		}catch(WidgetNotAvailableException ex){
			new WaitWhile(new JobIsRunning());
			open();
		}
		new WaitWhile(new JobIsRunning());
		
		return new SearchingForRuntimesDialog();
	}

	public void removeAllPaths(){
		Table table = new DefaultTable();

		int pathsNumber = table.rowCount();
		for (int i = 0; i < pathsNumber; i++){
			table.select(0);
			new PushButton("Remove").click();
		}
	}

	public SearchingForRuntimesDialog search(){
		new PushButton("Search...").click();
		new DefaultShell("Searching for runtimes...");
		new WaitUntil(new RuntimeSearchedFinished(), TimePeriod.LONG);
		return new SearchingForRuntimesDialog();
	}

	private static class RuntimeSearchedFinished implements WaitCondition {

		@Override
		public boolean test() {
			try {
				new DefaultLabel("Searching runtimes is finished.");
				return true;
			} catch (WidgetNotAvailableException e){
				return false;
			}
		}

		@Override
		public String description() {
			return "The runtime search has not finished in the specified amount of time";
		}
	}
}
