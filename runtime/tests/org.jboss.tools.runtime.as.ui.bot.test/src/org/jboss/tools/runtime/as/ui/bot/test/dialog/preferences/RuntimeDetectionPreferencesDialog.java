package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;
import org.jboss.tools.ui.bot.ext.condition.TaskDuration;

public class RuntimeDetectionPreferencesDialog extends PreferencesDialog{

	public void open(){
		open("JBoss Tools", "JBoss Tools Runtime Detection");
	}
	
	public SearchingForRuntimesDialog addPath(final String path){
		UIThreadRunnable.syncExec(new VoidResult() {
			
			@Override
			public void run() {
				RuntimeUIActivator.getDefault().getModel().addRuntimePath(new RuntimePath(path));
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
	
	public void removeAllPaths(){
		SWTBot bot = SWTBotFactory.getBot();
		SWTBotTable table = bot.table();
		
		int pathsNumber = table.rowCount();
		for (int i = 0; i < pathsNumber; i++){
			table.click(0, 0);
			bot.button("Remove").click();
		}
	}
	
	public SearchingForRuntimesDialog search(){
		SWTBotFactory.getBot().button("Search...").click();
		SWTBot bot = SWTBotFactory.getBot().shell("Searching for runtimes...").bot();
		bot.waitUntil(new RuntimeSearchedFinished(bot), TaskDuration.LONG.getTimeout());
		return new SearchingForRuntimesDialog();
	}
	
	private static class RuntimeSearchedFinished implements ICondition {

		private SWTBot bot;
		
		public RuntimeSearchedFinished(SWTBot bot) {
			this.bot = bot;
		}
		
		@Override
		public void init(SWTBot bot) {
		}
		
		@Override
		public boolean test() throws Exception {
			try {
				bot.label("Searching runtimes is finished.");
				return true;
			} catch (WidgetNotFoundException e){
				return false;
			}
		}

		@Override
		public String getFailureMessage() {
			return "The runtime search has not finished in the specified amount of time";
		}
	}
}
