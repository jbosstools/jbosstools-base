package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import java.util.Set;

import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Preference;
import org.jboss.tools.ui.bot.ext.gen.IPreference;

public class RuntimeDetectionPreferencesDialog {

	public void open(){
		IPreference preference = Preference.create("JBoss Tools", "JBoss Tools Runtime Detection");
		SWTBotFactory.getOpen().preferenceOpen(preference);
	}
	
	public SearchingForRuntimesDialog addPath(final String path){
		UIThreadRunnable.syncExec(new VoidResult() {
			
			@Override
			public void run() {
				Set<RuntimePath> runtimePaths = RuntimeUIActivator.getDefault().getRuntimePaths();
				runtimePaths.add(new RuntimePath(path));
				RuntimeUIActivator.getDefault().saveRuntimePaths();
			}
		});
		
		SWTBotFactory.getBot().button("OK").click();
		open();
		return new SearchingForRuntimesDialog();
	}
	
	public void removePath(final String path){
		SWTBotFactory.getBot().table().click(0, 0);
		SWTBotFactory.getBot().button("Remove").click();
	}
	
	public void ok(){
		SWTBotFactory.getBot().button("OK").click();
	}
	
	public SearchingForRuntimesDialog search(){
		SWTBotFactory.getBot().button("Search...").click();
		return new SearchingForRuntimesDialog();
	}
}
