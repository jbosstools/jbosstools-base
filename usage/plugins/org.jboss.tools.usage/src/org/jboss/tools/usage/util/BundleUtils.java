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

	/**
	 * Returns the bundles among the available ones that match the given filter.
	 * 
	 * @param filter
	 *            the filter to match the available bundles against
	 * @param bundleContext
	 *            the bundle context
	 * @return the bundles that match the given filter
	 */
	public static List<Bundle> getBundles(IBundleEntryFilter filter, BundleContext bundleContext) {
		List<Bundle> bundleList = new ArrayList<Bundle>();
		for (Bundle bundle : bundleContext.getBundles()) {
			if (filter.matches(bundle)) {
				bundleList.add(bundle);
			}
		}
		return bundleList;
	}

	/**
	 * Returns the bundles that have a symbolic name that match the given regex.
	 * 
	 * @param bundleSymbolicNameRegex
	 *            the symbolic name regex to match.
	 * @param bundleContext
	 *            the bundle context
	 * @return the bundles
	 */
	public static List<Bundle> getBundles(String bundleSymbolicNameRegex, BundleContext bundleContext) {
		return getBundles(new BundleSymbolicNameFilter(bundleSymbolicNameRegex), bundleContext);
	}

	/**
	 * A filter that matches bundles against a given symbolic name regex.
	 */
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

	/**
	 * A filter that applies several given filters
	 */
	public static class CompositeFilter implements IBundleEntryFilter {

		private IBundleEntryFilter filters[];

		/**
		 * Instantiates a new composite filter that applies several given
		 * filters.
		 * 
		 * @param filters
		 *            the filters
		 */
		public CompositeFilter(IBundleEntryFilter... filters) {
			this.filters = filters;
		}

		/**
		 * Applies the filters this composite filter has. All filters have to
		 * match so that the filter says the given bundle matches.
		 */
		@Override
		public boolean matches(Bundle bundle) {
			for (IBundleEntryFilter filter : filters) {
				if (!filter.matches(bundle)) {
					return false;
				}
			}
			return true;
		}
	}

	/**
	 * The Interface IBundleEntryFilter.
	 */
	public static interface IBundleEntryFilter {

		/**
		 * Matches.
		 * 
		 * @param bundle
		 *            the bundle
		 * @return true, if successful
		 */
		public boolean matches(Bundle bundle);
	}
}
