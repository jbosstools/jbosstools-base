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
package org.jboss.tools.stacks.core;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.osgi.service.debug.DebugOptions;
import org.eclipse.osgi.service.debug.DebugOptionsListener;

/**
 * Helper class to route trace output.
 */
public class Trace implements DebugOptionsListener {

	private static final SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yy HH:mm.ss.SSS"); //$NON-NLS-1$

	private static Set<String> logged = new HashSet<String>();

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
		Trace.CONFIG = options.getBooleanOption(StacksCoreActivator.PLUGIN_ID + Trace.STRING_CONFIG, false);
		Trace.INFO = options.getBooleanOption(StacksCoreActivator.PLUGIN_ID + Trace.STRING_INFO, false);
		Trace.WARNING = options.getBooleanOption(StacksCoreActivator.PLUGIN_ID + Trace.STRING_WARNING, false);
		Trace.SEVERE = options.getBooleanOption(StacksCoreActivator.PLUGIN_ID + Trace.STRING_SEVERE, false);
		Trace.FINER = options.getBooleanOption(StacksCoreActivator.PLUGIN_ID + Trace.STRING_FINER, false);
		Trace.FINEST = options.getBooleanOption(StacksCoreActivator.PLUGIN_ID + Trace.STRING_FINEST, false);
		Trace.RESOURCES = options.getBooleanOption(StacksCoreActivator.PLUGIN_ID + Trace.STRING_RESOURCES, false);
		Trace.EXTENSION_POINT = options.getBooleanOption(StacksCoreActivator.PLUGIN_ID + Trace.STRING_EXTENSION_POINT, false);
		Trace.LISTENERS = options.getBooleanOption(StacksCoreActivator.PLUGIN_ID + Trace.STRING_LISTENERS, false);
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
			if (!logged.contains(s)) {
				StacksCoreActivator.pluginLog().logError(s, t);
				logged.add(s);
			}
		}
		if (StacksCoreActivator.getDefault().isDebugging()) {
			final StringBuffer sb = new StringBuffer(StacksCoreActivator.PLUGIN_ID);
			sb.append(" "); //$NON-NLS-1$
			sb.append(level);
			sb.append(" "); //$NON-NLS-1$
			sb.append(sdf.format(new Date()));
			sb.append(" "); //$NON-NLS-1$
			sb.append(s);
			System.out.println(sb.toString());
			if (t != null) {
				t.printStackTrace();
			}
		}
	}

	
}