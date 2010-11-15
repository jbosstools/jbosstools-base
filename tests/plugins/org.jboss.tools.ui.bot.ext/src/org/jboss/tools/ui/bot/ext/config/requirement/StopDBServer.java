package org.jboss.tools.ui.bot.ext.config.requirement;

import org.jboss.tools.ui.bot.ext.config.TestConfigurator;
import org.jboss.tools.ui.bot.ext.helper.DatabaseHelper;

public class StopDBServer extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		if (TestConfigurator.currentConfig.getDB() != null)
			if (TestConfigurator.currentConfig.getDB().internal && DatabaseHelper.isHSQLDBRunning()) 
				return false;
		return true;
	}

	@Override
	public void handle() {
		DatabaseHelper.stopHSQLDBServer();		
	}
}
