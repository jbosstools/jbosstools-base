package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;
import org.jboss.tools.ui.bot.ext.SWTEclipseExt;

public class SearchingForRuntimesDialog {
	
	public List<Runtime> getRuntimes(){
		List<Runtime> runtimes = new ArrayList<Runtime>();
		
		for (SWTBotTreeItem treeItem : getRuntimesTreeItems()) {
			Runtime runtime = new Runtime();
			runtime.setName(treeItem.cell(0));
			runtime.setVersion(treeItem.cell(1));
			runtime.setType(treeItem.cell(2));
			runtime.setLocation(treeItem.cell(3));
			runtimes.add(runtime);
		}
		return runtimes;
	}
	
	public void ok(){
		SWTBotFactory.getBot().button("OK").click();
	}
	
	public void cancel(){
		SWTBotFactory.getBot().button("Cancel").click();
	}

	public void hideAlreadyCreatedRuntimes() {
		SWTBotFactory.getBot().checkBox("Hide already created runtimes").select();
	}
	
	public void deselect(String runtimeName){
		for (SWTBotTreeItem treeItem : getRuntimesTreeItems()) {
			if (treeItem.cell(0).equals(runtimeName)){
				treeItem.uncheck();
			}
		}
	}
	
	private List<SWTBotTreeItem> getRuntimesTreeItems(){
		SWTBotTree tree = SWTBotFactory.getBot().tree();
		return SWTEclipseExt.getAllTreeItemsRecursive(SWTBotFactory.getBot(), tree, true);
	}
}
