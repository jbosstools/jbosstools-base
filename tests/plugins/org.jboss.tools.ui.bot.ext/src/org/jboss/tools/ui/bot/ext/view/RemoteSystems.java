package org.jboss.tools.ui.bot.ext.view;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.View;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

public class RemoteSystems extends ViewBase {
	Logger log = Logger.getLogger(RemoteSystems.class);

	public RemoteSystems() {
		viewObject = View.create("Remote Systems","Remote Systems");		
	}
	public boolean existsConnection(String name) {
		try {
			bot().tree().getTreeItem(name);
			return true;
		}
		catch (WidgetNotFoundException e) {
			return false;
		}
	}
	public List<String> getConnections() {
		List<String> list = new ArrayList<String>();
		for (SWTBotTreeItem item : bot().tree().getAllItems()) {
			list.add(item.getText());
		}
		return list;
	}
	public void connect(String connection,String username, String password) {
		bot().tree().getTreeItem(connection).select();
		bot().tree().contextMenu("Connect").click();
		SWTBot shell = bot.shell("Enter password").bot();
		if (username!=null)
			shell.textWithLabel("User ID:").setText(username);
		if (password!=null)
			shell.textWithLabel("Password (optional):").setText(password);
		shell.button(IDELabel.Button.OK).click();
		util.waitForNonIgnoredJobs();
	}
	public void disconnect(String connection) {
		
	}
	public void delete(String connection) {
		
	}
}
