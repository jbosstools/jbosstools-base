/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 *     Zend Technologies Ltd. - JBIDE-18678
 ******************************************************************************/
package org.jboss.tools.usage.test;

import org.jboss.tools.usage.impl.JBossToolsUsageImplActivator;
import org.jboss.tools.usage.tracker.internal.FocusPoint;
import org.jboss.tools.usage.tracker.internal.SuffixFocusPoint;

/**
 * @author Andre Dietisheim
 * @author Kaloyan Raev
 */
public class JBossToolsTestsFocusPoint extends SuffixFocusPoint {

	public JBossToolsTestsFocusPoint(String childFocusPoint) {
		super("tools", getJBossToolsVersion());
		setChild(new FocusPoint("usage")
				.setChild(new FocusPoint("tests")
						.setChild(new FocusPoint(childFocusPoint))));
	}

	public static String getJBossToolsVersion() {
		return JBossToolsUsageImplActivator.getDefault().getBundle().getVersion().toString();
	}
}
