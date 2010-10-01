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
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;

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

//	@SuppressWarnings("unchecked")
//	public static <T> T getService(String serviceClass, Bundle bundle) {
//		BundleContext bundleContext = bundle.getBundleContext();
//		ServiceReference serviceReference = bundleContext.getServiceReference(serviceClass);
//		if (serviceReference == null) {
//			return null;
//		}
//		return (T) bundleContext.getService(serviceReference);
//	}

	@SuppressWarnings("unchecked")
	public static <T> T getHighestRankedService(String serviceClass, Bundle bundle) throws InvalidSyntaxException {
		BundleContext bundleContext = bundle.getBundleContext();
		ServiceReference[] serviceReferences = bundleContext.getServiceReferences(serviceClass, null);
		if (serviceReferences.length != 0) {
			ServiceReference reference = getHihgestRankedServiceReference(bundleContext, serviceReferences);
			if (reference != null) {
				return (T) bundleContext.getService(reference);
			}
		}
		
		return null;
	}

	private static ServiceReference getHihgestRankedServiceReference(BundleContext bundleContext,
			ServiceReference[] serviceReferences) {
		Integer highestRanking = Integer.MIN_VALUE;
		ServiceReference currentReference = null;
		for (ServiceReference reference : serviceReferences) {
			Integer ranking = (Integer) reference.getProperty(Constants.SERVICE_RANKING);
			if (ranking > highestRanking) {
				currentReference = reference;
				highestRanking = ranking;
			}
		}
		return currentReference;
	}
}
