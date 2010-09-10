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


/**
 * @author Andre Dietisheim
 */
public class CollectionFilterUtils {

	/**
	 * A filter that applies several given filters
	 */
	public static class CompositeCollectionFilter<E> implements ICollectionEntryFilter<E> {

		private ICollectionEntryFilter<E> filters[];

		/**
		 * Instantiates a new composite filter that applies several given
		 * filters.
		 * 
		 * @param filters
		 *            the filters
		 */
		public CompositeCollectionFilter(ICollectionEntryFilter<E>... filters) {
			this.filters = filters;
		}

		/**
		 * Applies the filters this composite filter has. All filters have to
		 * match so that the filter says the given bundle matches.
		 */
		public boolean matches(E entry) {
			for (ICollectionEntryFilter<E> filter : filters) {
				if (!filter.matches(entry)) {
					return false;
				}
			}
			return true;
		}
	}
}
