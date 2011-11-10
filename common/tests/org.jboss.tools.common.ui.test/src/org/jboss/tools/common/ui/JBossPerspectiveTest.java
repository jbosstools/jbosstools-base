/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.ui;

import static org.junit.Assert.assertNotNull;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.WorkbenchException;
import org.jboss.tools.test.util.WorkbenchUtils;
import org.junit.Test;

/**
 * @author Alexey Kazakov
 */
public class JBossPerspectiveTest {

	@Test
	public void testPerspective() throws WorkbenchException {
		IWorkbenchPage page = WorkbenchUtils.getWorkbench().getActiveWorkbenchWindow().openPage(JBossPerspectiveFactory.PERSPECTIVE_ID, null);
		assertNotNull(page);
	}
}