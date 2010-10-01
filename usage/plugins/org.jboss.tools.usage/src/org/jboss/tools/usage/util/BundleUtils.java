/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.util;

import java.util.Collection;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Assert;
import org.jboss.tools.usage.util.collectionfilter.CollectionFilterUtils;
import org.jboss.tools.usage.util.collectionfilter.ICollectionFilter;
import org.osgi.framework.Bundle;

/**
 * @author Andre Dietisheim
 */
public class BundleUtils {

	/**
	 * Returns the bundles among the available ones that match the given filter.
	 * 
	 * @param filter
	 *            the filter to match the available bundles against
	 * @param bundles
	 *            the bundles
	 * @return the bundles that match the given filter
	 */
	public static void getBundles(ICollectionFilter<Bundle> filter, Collection<Bundle> filteredBundleCollection,
			Bundle[] bundles) {
		CollectionFilterUtils.filter(filter, bundles, filteredBundleCollection);
	}

	/**
	 * Returns the bundles that have a symbolic name that match the given regex.
	 * 
	 * @param bundleSymbolicNameRegex
	 *            the symbolic name regex to match.
	 * @param bundles
	 *            the bundles
	 * @return the bundles
	 */
	public static void getBundles(String bundleSymbolicNameRegex, Collection<Bundle> filteredBundleCollection,
			Bundle[] bundles) {
		getBundles(new BundleSymbolicNameFilter(bundleSymbolicNameRegex)
				, filteredBundleCollection
				, bundles);
	}

	/**
	 * A filter that matches bundles against a given symbolic name regex.
	 */
	public static class BundleSymbolicNameFilter implements ICollectionFilter<Bundle> {

		private Pattern pattern;

		public BundleSymbolicNameFilter(String symbolicNameRegex) {
			this.pattern = Pattern.compile(symbolicNameRegex);
		}

		public boolean matches(Bundle bundle) {
			Assert.isTrue(bundle != null);

			return pattern.matcher(bundle.getSymbolicName()).matches();
		}
	}
}
