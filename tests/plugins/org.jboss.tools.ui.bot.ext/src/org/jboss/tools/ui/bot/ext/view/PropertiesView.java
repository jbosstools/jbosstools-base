package org.jboss.tools.ui.bot.ext.view;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;

public class PropertiesView extends ViewBase {
	Logger log = Logger.getLogger(PropertiesView.class);
	public PropertiesView() {
		viewObject = ActionItem.View.GeneralProperties.LABEL;		
	}
	public List<String> getPropertyKeys() {
		List<String> list = new ArrayList<String>();
		for (SWTBotTreeItem item : this.show().bot().tree().getAllItems()) {
			list.add(item.cell(0));
		}
		return list;
	}
	public String getPropertyValue(String property) {
		for (SWTBotTreeItem item : this.show().bot().tree().getAllItems()) {
			if (item.cell(0).equals(property)) {
				return item.cell(1);
			}
		}
		return null;
	}
}
