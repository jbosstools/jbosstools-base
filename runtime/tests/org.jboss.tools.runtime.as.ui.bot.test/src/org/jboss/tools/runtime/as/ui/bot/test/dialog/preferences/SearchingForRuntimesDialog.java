package org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Server;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;

public class SearchingForRuntimesDialog {
	
	public List<Server> getServers(){
		List<Server> servers = new ArrayList<Server>();
		SWTBotTree tree = SWTBotFactory.getBot().tree();
		
		for (int i = 0; i < tree.rowCount(); i++){
			Server server = new Server();
			server.setName(tree.cell(i, 0));
			server.setVersion(tree.cell(i, 1));
			server.setType(tree.cell(i, 2));
			server.setLocation(tree.cell(i, 3));
			servers.add(server);
		}
		return servers;
	}
	
	public void ok(){
		SWTBotFactory.getBot().button("OK").click();
	}
}
