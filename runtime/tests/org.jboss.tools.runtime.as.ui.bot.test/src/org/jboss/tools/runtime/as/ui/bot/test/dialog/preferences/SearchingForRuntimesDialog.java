package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import java.util.ArrayList;
import java.util.List;

import org.jboss.reddeer.swt.api.Shell;
import org.jboss.reddeer.swt.api.TreeItem;
import org.jboss.reddeer.swt.condition.ShellWithTextIsActive;
import org.jboss.reddeer.swt.impl.button.CheckBox;
import org.jboss.reddeer.swt.impl.button.PushButton;
import org.jboss.reddeer.swt.impl.shell.DefaultShell;
import org.jboss.reddeer.swt.impl.tree.DefaultTree;
import org.jboss.reddeer.swt.wait.WaitWhile;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;

public class SearchingForRuntimesDialog {
	
	public List<Runtime> getRuntimes(){
		List<Runtime> runtimes = new ArrayList<Runtime>();
		
		for (TreeItem treeItem : getRuntimesTreeItems()) {
			Runtime runtime = new Runtime();
			runtime.setName(treeItem.getCell(0));
			runtime.setVersion(treeItem.getCell(1));
			runtime.setType(treeItem.getCell(2));
			runtime.setLocation(treeItem.getCell(3));
			runtimes.add(runtime);
		}
		return runtimes;
	}
	
	public void ok(){
		Shell shell = new DefaultShell();
		new PushButton("OK").click();
		new WaitWhile(new ShellWithTextIsActive(shell.getText()));
	}
	
	public void cancel(){
		new PushButton("Cancel").click();
	}

	public void hideAlreadyCreatedRuntimes() {
		new CheckBox("Hide already created runtimes").toggle(true);
	}
	
	public void deselect(String runtimeName){
		for (TreeItem treeItem : getRuntimesTreeItems()) {
			if (treeItem.getCell(0).equals(runtimeName)){
				treeItem.setChecked(false);
			}
		}
	}
	
	private List<TreeItem> getRuntimesTreeItems(){
		return new DefaultTree().getAllItems();
	}
}
