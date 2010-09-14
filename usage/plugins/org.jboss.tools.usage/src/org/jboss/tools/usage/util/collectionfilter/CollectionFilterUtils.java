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
package org.jboss.tools.usage.util.collectionfilter;

import java.util.Collection;

/**
 * @author Andre Dietisheim
 */
public class CollectionFilterUtils {

	public static <E, T> void filter(ICollectionFilter<E> filter, E[] entries, Collection<T> targetColletion) {
		filter(filter, new NoopConverter<E, T>(), entries, targetColletion);
	}

	/**
	 * Returns the entries that match the given filter.
	 * 
	 * @param filter
	 *            the filter to match the available entries against
	 * @param entries
	 *            the entries to filter
	 * @return the entries that match the given filter
	 */
	public static <E, T> void filter(ICollectionFilter<E> filter, ICollectionEntryConverter<E, T> converter,
			E[] entries, Collection<T> targetColletion) {
		for (E entry : entries) {
			if (filter.matches(entry)) {
				if (targetColletion != null) {
					targetColletion.add(converter.convert(entry));
				}
			}
		}
	}
}
