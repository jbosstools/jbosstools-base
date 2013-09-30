/*******************************************************************************
 * Copyright (c) 2003, 2011 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - Initial API and implementation
 *******************************************************************************/
package org.jboss.tools.runtime.ui.internal;

import org.eclipse.osgi.service.debug.DebugOptions;
import org.eclipse.osgi.service.debug.DebugOptionsListener;
import org.jboss.tools.foundation.core.plugin.AbstractTrace;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * Helper class to route trace output.
 */
public class Trace extends AbstractTrace implements DebugOptionsListener {
	private static Trace instance = null;
	/**
	 * Trace constructor. This should never be explicitly called by clients and is used to register this class with the
	 * {@link DebugOptions} service.
	 */
	public Trace(RuntimeUIActivator p) {
		super(p);
		instance = this;
	}

	public static void trace(final String level, String s) {
		trace(level, s, null);
	}

	public static void trace(final String level, String s, Throwable t) {
		traceInternal(instance, level, s, t);
	}
}