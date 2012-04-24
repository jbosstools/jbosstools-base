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
/**
 * Parent test for tests with seam requirement
 * @author lzoubek
 *
 */
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.*;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
import org.junit.Test;

@Require(seam=@Seam(),perspective="Java EE")
public class AnnotatedWithSeam extends SWTTestExt {
  /**
   * Checks seam configuration state
   */
	@Test
	public void configuredState() {
		assertTrue(configuredState.getSeam().isConfiured);
		assertNotNull(configuredState.getSeam().version);
		assertNotNull(configuredState.getSeam().name);

	}
	 /**
   * Checks seam runtime existence
   */
	@Test
	public void runtimeExists() {
		boolean found=false;
		SWTBot bot = open.preferenceOpen(ActionItem.Preference.JBossToolsWebSeam.LABEL);
		SWTBotTable tbRuntimeEnvironments = bot.table();
		// first check if Environment doesn't exist
		int numRows = tbRuntimeEnvironments.rowCount();
		if (numRows > 0) {
			int currentRow = 0;
			while (currentRow < numRows) {
				if (tbRuntimeEnvironments.cell(currentRow, 1).equalsIgnoreCase(
						configuredState.getSeam().name)) {
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
