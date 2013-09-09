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
package org.jboss.tools.foundation.core;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.osgi.service.debug.DebugOptions;
import org.eclipse.osgi.service.debug.DebugOptionsListener;

/**
 * Helper class to route trace output.
 */
public class Trace implements DebugOptionsListener {
	// tracing enablement flags
	public static boolean CONFIG = false;
	public static boolean INFO = false;
	public static boolean WARNING = false;
	public static boolean SEVERE = false;
	public static boolean FINER = false;
	public static boolean FINEST = false;
	public static boolean RESOURCES = false;
	public static boolean EXTENSION_POINT = false;
	public static boolean LISTENERS = false;

	// tracing levels. One most exist for each debug option
	public final static String STRING_CONFIG = "/config"; //$NON-NLS-1$
	public final static String STRING_INFO = "/info"; //$NON-NLS-1$
	public final static String STRING_WARNING = "/warning"; //$NON-NLS-1$
	public final static String STRING_SEVERE = "/severe"; //$NON-NLS-1$
	public final static String STRING_FINER = "/finer"; //$NON-NLS-1$
	public final static String STRING_FINEST = "/finest"; //$NON-NLS-1$
	public final static String STRING_RESOURCES = "/resources"; //$NON-NLS-1$
	public final static String STRING_EXTENSION_POINT = "/extension_point"; //$NON-NLS-1$
	public final static String STRING_LISTENERS = "/listeners"; //$NON-NLS-1$
	
	/**
	 * Trace constructor. This should never be explicitly called by clients and is used to register this class with the
	 * {@link DebugOptions} service.
	 */
	public Trace() {
		super();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.osgi.service.debug.DebugOptionsListener#optionsChanged(org.eclipse.osgi.service.debug.DebugOptions)
	 */
	public void optionsChanged(DebugOptions options) {
		Trace.CONFIG = options.getBooleanOption(FoundationCorePlugin.PLUGIN_ID + Trace.STRING_CONFIG, false);
		Trace.INFO = options.getBooleanOption(FoundationCorePlugin.PLUGIN_ID + Trace.STRING_INFO, false);
		Trace.WARNING = options.getBooleanOption(FoundationCorePlugin.PLUGIN_ID + Trace.STRING_WARNING, false);
		Trace.SEVERE = options.getBooleanOption(FoundationCorePlugin.PLUGIN_ID + Trace.STRING_SEVERE, false);
		Trace.FINER = options.getBooleanOption(FoundationCorePlugin.PLUGIN_ID + Trace.STRING_FINER, false);
		Trace.FINEST = options.getBooleanOption(FoundationCorePlugin.PLUGIN_ID + Trace.STRING_FINEST, false);
		Trace.RESOURCES = options.getBooleanOption(FoundationCorePlugin.PLUGIN_ID + Trace.STRING_RESOURCES, false);
		Trace.EXTENSION_POINT = options.getBooleanOption(FoundationCorePlugin.PLUGIN_ID + Trace.STRING_EXTENSION_POINT, false);
		Trace.LISTENERS = options.getBooleanOption(FoundationCorePlugin.PLUGIN_ID + Trace.STRING_LISTENERS, false);
	}

	/**
	 * Trace the given message.
	 * 
	 * @param level
	 *            The tracing level.
	 * @param s
	 *            The message to trace
	 */
	public static void trace(final String level, String s) {

		Trace.trace(level, s, null);
	}

	/**
	 * Trace the given message and exception.
	 * 
	 * @param level
	 *            The tracing level.
	 * @param s
	 *            The message to trace
	 * @param t
	 *            A {@link Throwable} to trace
	 */
	public static void trace(final String level, String s, Throwable t) {
		if (s == null) {
			return;
		}
		if (Trace.STRING_SEVERE.equals(level)) {
			FoundationCorePlugin.pluginLog().logError(s, t);
		}
		if (FoundationCorePlugin.getDefault().isDebugging()) {
			final StringBuilder sb = new StringBuilder(FoundationCorePlugin.PLUGIN_ID);
			sb.append(" "); //$NON-NLS-1$
			sb.append(level);
			sb.append(" "); //$NON-NLS-1$
			sb.append(formatDate());
			sb.append(" "); //$NON-NLS-1$
			sb.append(s);
			System.out.println(sb.toString());
			if (t != null) {
				t.printStackTrace();
			}
		}
	}

	private static String formatDate() {
		SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yy HH:mm.ss.SSS"); //$NON-NLS-1$
		return sdf.format(new Date());
	}
}