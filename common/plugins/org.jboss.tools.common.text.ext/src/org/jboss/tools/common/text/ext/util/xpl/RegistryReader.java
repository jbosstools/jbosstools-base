/*******************************************************************************
 * Copyright (c) 2001, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Jens Lukowski/Innoopract - initial renaming/restructuring
 *     Exadel, Inc.
 *     Red Hat, Inc.     
 *******************************************************************************/
package org.jboss.tools.common.text.ext.util.xpl;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.wst.sse.ui.internal.util.Sorter;

/**
 */
public abstract class RegistryReader extends org.eclipse.wst.sse.ui.internal.extension.RegistryReader {

	/**
	 * The constructor.
	 */
	protected RegistryReader() {
	}

	/**
	 * Override to improve sorting
	 */
	protected IExtension[] orderExtensions(IExtension[] extensions) {
		Object[] sorted = createSorter().sort(extensions);
		IExtension[] sortedExtension = new IExtension[sorted.length];
		System.arraycopy(sorted, 0, sortedExtension, 0, sorted.length);
		return sortedExtension;
	}
	
	protected Sorter createSorter() {
		return new Sorter() {
			public boolean compare(Object extension1, Object extension2) {
				String s1 = ((IExtension) extension1).getUniqueIdentifier().toUpperCase();
				String s2 = ((IExtension) extension2).getUniqueIdentifier().toUpperCase();
				return s2.compareTo(s1) > 0;
			}
		};
	}

	protected void logUnknownElement(IConfigurationElement element) {
		//do nothing
	}

}
