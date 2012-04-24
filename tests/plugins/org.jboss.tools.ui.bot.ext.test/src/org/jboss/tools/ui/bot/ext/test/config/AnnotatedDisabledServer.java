 /*******************************************************************************
  * Copyright (c) 2007-2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.test.config;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.Require;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerState;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.junit.Test;
/**
 * Parent test for tests using server in disabled state. 
 * n state when server is not defined within workspace.
 * @author lzoubek
 *
 */
@Require(server=@Server(state=ServerState.Disabled))
public class AnnotatedDisabledServer extends SWTTestExt {
  /**
   * Checks server state
   */
	@Test
	public void configuredState() {
		assertTrue(!configuredState.getServer().isRunning);
		assertTrue(!configuredState.getServer().isConfigured);
		assertNull(configuredState.getServer().version);
		assertNull(configuredState.getServer().type);
		assertNull(configuredState.getServer().name);
		assertNull(configuredState.getServer().withJavaVersion);
	}
	/**
   * Checks server doesn't exist
   */
	@Test
	public void serverDoesNotExist() {
		assertTrue(!servers.show().bot().tree().hasItems());
	}
  /**
   * Checks server runtime doesn't exist
   */
	@Test
	public void runtimeDoesNotExist() {
		SWTBot wiz = open
		.preferenceOpen(ActionItem.Preference.ServerRuntimeEnvironments.LABEL);
		SWTBotTable tbRuntimeEnvironments = bot.table();
		assertTrue(tbRuntimeEnvironments.rowCount()==0);
		open.closeCancel(wiz);
	}

}
