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
import org.jboss.tools.ui.bot.ext.config.Annotations.ESB;
import org.jboss.tools.ui.bot.ext.config.Annotations.Require;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.junit.Test;
/**
 * Parent test for tests with ESB requirement
 * @author lzoubek
 *
 */
@Require(esb=@ESB(),perspective="Java EE")
public class AnnotatedWithESB extends SWTTestExt {
  /**
   * Checks ESB configuration state
   */
	@Test
	public void configuredState() {
		assertTrue(configuredState.getEsb().isConfiured);
		assertNotNull(configuredState.getEsb().version);
		assertNotNull(configuredState.getEsb().name);
	}
	 /**
   * Checks ESB runtime existence
   */
	@Test
	public void runtimeExists() {
		boolean found=false;
		SWTBot bot = open.preferenceOpen(ActionItem.Preference.JBossToolsJBossESBRuntimes.LABEL);
		SWTBotTable tbRuntimeEnvironments = bot.table();
		// first check if Environment doesn't exist
		int numRows = tbRuntimeEnvironments.rowCount();
		if (numRows > 0) {
			int currentRow = 0;
			while (currentRow < numRows) {
				if (tbRuntimeEnvironments.cell(currentRow, 1).equalsIgnoreCase(
						configuredState.getEsb().name)) {
					found = true;
					break;
				} else {
					currentRow++;
				}
			}
		}
		assertTrue(found);
	}

}
