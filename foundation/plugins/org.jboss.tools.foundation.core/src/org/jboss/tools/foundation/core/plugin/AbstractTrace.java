/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.core.plugin;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Plugin;
import org.eclipse.osgi.service.debug.DebugOptions;
import org.eclipse.osgi.service.debug.DebugOptionsListener;
import org.jboss.tools.foundation.core.plugin.log.StatusFactory;

/**
 * In an attempt to unify all the nearly-identical Trace classes that are everywhere, 
 * this class is born. Clients may override various methods to allow for
 * further categories for their own use cases. 
 * @since 1.1
 */
public class AbstractTrace implements DebugOptionsListener {

	// tracing levels. One most exist for each debug option
	public static final String STRING_CONFIG = "/config"; //$NON-NLS-1$
	public static final String STRING_INFO = "/info"; //$NON-NLS-1$
	public static final String STRING_WARNING = "/warning"; //$NON-NLS-1$
	public static final String STRING_SEVERE = "/severe"; //$NON-NLS-1$
	public static final String STRING_FINER = "/finer"; //$NON-NLS-1$
	public static final String STRING_FINEST = "/finest"; //$NON-NLS-1$
	public static final String STRING_RESOURCES = "/resources"; //$NON-NLS-1$
	public static final String STRING_EXTENSION_POINT = "/extension_point"; //$NON-NLS-1$
	public static final String STRING_LISTENERS = "/listeners"; //$NON-NLS-1$
	
	private Plugin plugin;
	private Map<String, Boolean> optionsMap = new HashMap<>();
	
	/**
	 * Trace constructor. This should never be explicitly called by clients and is used to register this class with the
	 * {@link DebugOptions} service.
	 */
	protected AbstractTrace(Plugin plugin) {
		super();
		this.plugin = plugin;
		createDefaultDebugSettings();
	}
	
	/**
	 * Add to a map a string (from your .options file) with an 
	 * initial Boolean value of false. Do this for all of your options
	 */
	protected void createDefaultDebugSettings() {
		getOptionsMap().put(STRING_CONFIG, new Boolean(false));
		getOptionsMap().put(STRING_INFO, new Boolean(false));
		getOptionsMap().put(STRING_WARNING, new Boolean(false));
		getOptionsMap().put(STRING_SEVERE, new Boolean(false));
		getOptionsMap().put(STRING_FINER, new Boolean(false));
		getOptionsMap().put(STRING_FINEST, new Boolean(false));
		getOptionsMap().put(STRING_RESOURCES, new Boolean(false));
		getOptionsMap().put(STRING_EXTENSION_POINT, new Boolean(false));
		getOptionsMap().put(STRING_LISTENERS, new Boolean(false));
	}

	/**
	 * Fetch the current settings from the osgi debug service.
	 * Clients may override this to also check their own custom 
	 * trace levels. 
	 * 
	 * @see
	 * org.eclipse.osgi.service.debug.DebugOptionsListener#optionsChanged(org.eclipse.osgi.service.debug.DebugOptions)
	 */
	@Override
	public void optionsChanged(DebugOptions options) {
		String pid = plugin.getBundle().getSymbolicName();
		getOptionsMap().put(STRING_CONFIG, options.getBooleanOption(pid + STRING_CONFIG, false));
		getOptionsMap().put(STRING_INFO, options.getBooleanOption(pid + STRING_INFO, false));
		getOptionsMap().put(STRING_WARNING, options.getBooleanOption(pid + STRING_WARNING, false));
		getOptionsMap().put(STRING_SEVERE, options.getBooleanOption(pid + STRING_SEVERE, false));
		getOptionsMap().put(STRING_FINER, options.getBooleanOption(pid + STRING_FINER, false));
		getOptionsMap().put(STRING_FINEST, options.getBooleanOption(pid + STRING_FINEST, false));
		getOptionsMap().put(STRING_RESOURCES, options.getBooleanOption(pid + STRING_RESOURCES, false));
		getOptionsMap().put(STRING_EXTENSION_POINT, options.getBooleanOption(pid + STRING_EXTENSION_POINT, false));
		getOptionsMap().put(STRING_LISTENERS, options.getBooleanOption(pid + STRING_LISTENERS, false));
	}

	/**
	 * Trace the given message.
	 * 
	 * @param at   the AbstractTrace object for context
	 * @param level  The tracing level.
	 * @param s   The message to trace
	 */
	protected static void traceInternal(AbstractTrace at, String level, String s) {
		traceInternal(at, level, s, null);
	}
	
	/**
	 * Trace the given message and exception.
	 * 
	 * @param at   the AbstractTrace object for context
	 * @param level  The tracing level.
	 * @param s   The message to trace
	 * @param t   A {@link Throwable} to trace
	 */
	protected static void traceInternal(AbstractTrace at, final String level, String s, Throwable t) {
		// Check conditions to not trace
		if( level == null || at == null)
			return;
		
		Boolean val = at.optionsMap.get(level);
		if( val == null || !val.booleanValue())
			return;
		if (s == null) {
			return;
		}
		
		
		if (STRING_SEVERE.equals(level)) {
			at.plugin.getLog().log(StatusFactory.errorStatus(at.plugin.getBundle().getSymbolicName(), s, t));
		}
		if (at.plugin.isDebugging()) {
			final StringBuilder sb = new StringBuilder(at.plugin.getBundle().getSymbolicName());
			sb.append(" ") //$NON-NLS-1$
			.append(level)
			.append(" ") //$NON-NLS-1$
			.append(formatDate())
			.append(" ") //$NON-NLS-1$
			.append(s);
			
			System.out.println(sb.toString());
			if (t != null) {
				t.printStackTrace();
			}
		}
	}
	
	
	
	protected static String formatDate() {
		SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yy HH:mm.ss.SSS"); //$NON-NLS-1$
		return sdf.format(new Date());
	}

	protected Map<String, Boolean> getOptionsMap() {
		return optionsMap;
	}
}
