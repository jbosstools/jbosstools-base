/*******************************************************************************
 * Copyright (c) 2008 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.jboss.tools.usage.util.BundleUtils.IBundleEntryFilter;
import org.osgi.framework.Bundle;

public class JBossBundleGroups implements IBundleEntryFilter {

	private static final String SEAM_ID = "seam";
	private static final String SMOOKS_ID = "smooks";

	private Set<String> jbossBundleGroups = new HashSet<String>();

	@Override
	public boolean matches(Bundle bundle) {
		String bundleName = bundle.getSymbolicName();
		if (isSmooks(bundleName)) {
			addGroup(SMOOKS_ID);
		} else if (isSeam(bundleName)) {
			addGroup(SEAM_ID);
		}
		return false;
	}

	private void addGroup(String groupId) {
		jbossBundleGroups.add(groupId);
	}

	private boolean isSeam(String bundleName) {
		return "org.jboss.tools.seam.core".equals(bundleName)
				| "org.jboss.tools.seam.doc.user".equals(bundleName)
				| "org.jboss.tools.seam.pages.xml".equals(bundleName)
				| "org.jboss.tools.seam.text.ext".equals(bundleName)
				| "org.jboss.tools.seam.ui".equals(bundleName)
				| "org.jboss.tools.seam.ui.pages".equals(bundleName)
				| "org.jboss.tools.seam.xml".equals(bundleName)
				| "org.jboss.tools.seam.xml.ui".equals(bundleName);
	}

	private boolean isSmooks(String bundleName) {
		return "org.jboss.tools.smooks.core".equals(bundleName)
				| "org.jboss.tools.smooks.runtime".equals(bundleName)
				| "org.jboss.tools.smooks.templating".equals(bundleName)
				| "org.jboss.tools.smooks.ui".equals(bundleName);
	}
	
	public Collection<String> getBundleGroupIds() {
		return jbossBundleGroups;
	}

}