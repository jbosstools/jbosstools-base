/*******************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. Distributed under license by Red Hat, Inc.
 * All rights reserved. This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui;

import static org.junit.Assert.*;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.junit.Test;
import  org.jboss.tools.common.ui.JobUtils;

public class JobUtilsTest {

	private IStatus status;
	
	@Test
	public void isWarningShouldBeFalseWhenNull() {
		assertFalse(JobUtils.isWarning(null));
	}
	@Test
	public void isWarningShouldBeTrueWhenNotNullAndWarningSeverity() {
		status = new Status(IStatus.WARNING, "a plugin name", "");
		assertTrue(JobUtils.isWarning(status));
	}
	@Test
	public void isWarningShouldBeFalseWhenNotNullAndNotWarningSeverity() {
		status = new Status(IStatus.INFO, "a plugin name", "");
		assertFalse(JobUtils.isWarning(status));
	}

}
