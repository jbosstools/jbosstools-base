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
package org.jboss.tools.usage.util;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Assert;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

public class BundleUtils {

	public static List<Bundle> getBundles(String symbolicNameRegex, BundleContext bundleContext) {
		List<Bundle> bundleList = new ArrayList<Bundle>();
		BundleSymbolicNameFilter symbolicNameFilter = new BundleSymbolicNameFilter(symbolicNameRegex);
		for (Bundle bundle : bundleContext.getBundles()) {
			if (symbolicNameFilter.matches(bundle)) {
				bundleList.add(bundle);
			}
		}
		return bundleList;
	}

	public static class BundleSymbolicNameFilter implements IBundleEntryFilter {

		private Pattern pattern;

		public BundleSymbolicNameFilter(String symbolicNameRegex) {
			this.pattern = Pattern.compile(symbolicNameRegex);
		}

		@Override
		public boolean matches(Bundle bundle) {
			Assert.isTrue(bundle != null);

			return pattern.matcher(bundle.getSymbolicName()).matches();
		}

	}

	public static interface IBundleEntryFilter {
		public boolean matches(Bundle bundle);
	}
}
