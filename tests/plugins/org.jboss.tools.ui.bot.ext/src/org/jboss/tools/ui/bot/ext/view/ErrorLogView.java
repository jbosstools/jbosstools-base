package org.jboss.tools.ui.bot.ext.view;

import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.jboss.tools.ui.bot.ext.helper.ContextMenuHelper;

/**
 * Error Log view SWTBot Extensions
 * 
 * @author jpeterka
 * 
 */
public class ErrorLogView extends ViewBase {

	/**
	 * logs all error log messages into a logger
	 */
	public void logMessages() {
		SWTBotTreeItem[] items = getView().bot().tree().getAllItems();
		for (SWTBotTreeItem i : items)
			passTree(i);
	}

	private void passTree(SWTBotTreeItem item) {
		int i = 0;
		item.expand();
		while (!item.getNodes().isEmpty() && i < item.getNodes().size()) {
			passTree(item.getNode(i));
			i++;
		}
		log.info(item.getText());
	}

	/**
	 * clear error log (temporarily)
	 */
	public void clear() {
		ContextMenuHelper.clickContextMenu(getView().bot().tree(),
				"Clear Log Viewer");
		
		log.info("Error log cleared");
	}
	
	/**
	 * deletes error log permanently
	 */
	public void delete() {
		ContextMenuHelper.clickContextMenu(getView().bot().tree(),
				"Delete Log");
		
		SWTBotShell shell = bot.shell("Confirm Delete").activate();
		shell.bot().button("OK").click();
		bot.waitUntil(Conditions.shellCloses(shell));
		log.info("Error log content deleted");		
	}

	private SWTBotView getView() {
		SWTBotView view = open.viewOpen(ActionItem.View.GeneralErrorLog.LABEL);
		return view;
	}

	/**
	 * 
	 * @return all item count in the error log tree
	 */
	public int getRecordCount() {
		int count = getView().bot().tree().getAllItems().length;
		return count;
	}

}
