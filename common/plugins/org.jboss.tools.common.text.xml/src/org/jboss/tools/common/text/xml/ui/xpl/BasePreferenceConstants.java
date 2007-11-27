/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc.
 *******************************************************************************/
package org.jboss.tools.common.text.xml.ui.xpl;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.texteditor.AbstractTextEditor;

import org.jboss.tools.common.text.xml.XmlEditorPlugin;

/**
 * @author Jeremy
 *
 */
public class BasePreferenceConstants {
	
	/**
	 * Returns the preference store.
	 *
	 * @return the preference store
	 */
	public static IPreferenceStore getPreferenceStore() {
		return XmlEditorPlugin.getDefault().getPreferenceStore();
	}

	/**
	 * Preference key suffix for bold text style preference keys.
	 *
	 * @since 2.1
	 */
	public static final String EDITOR_BOLD_SUFFIX= "_bold"; //$NON-NLS-1$
	public static final String EDITOR_FOREGROUND_SUFFIX = "_foreground"; //$NON-NLS-1$
	public static final String EDITOR_BACKGROUND_SUFFIX = "_background"; //$NON-NLS-1$
	public static final String EDITOR_CUSTOM_BACKGROUND_SUFFIX = "_custom_background"; //$NON-NLS-1$

	/**
	 * A named preference that holds the color used as the text background.
	 * This value has not effect if the system default color is used.
	 * <p>
	 * Value is of type <code>String</code>. A RGB color value encoded as a string
	 * using class <code>PreferenceConverter</code>
	 * </p>
	 *
	 * @see org.eclipse.jface.resource.StringConverter
	 * @see org.eclipse.jface.preference.PreferenceConverter
	 */
	public final static String EDITOR_BACKGROUND_COLOR= AbstractTextEditor.PREFERENCE_COLOR_BACKGROUND;

	/**
	 * A named preference that describes if the system default background color
	 * is used as the text background.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public final static String EDITOR_BACKGROUND_DEFAULT_COLOR= AbstractTextEditor.PREFERENCE_COLOR_BACKGROUND_SYSTEM_DEFAULT;

	public final static String getItemForegroundKey (String item) {
		return item + EDITOR_FOREGROUND_SUFFIX;
	}
	public final static String getItemBackgroundKey (String item) {
		return item + EDITOR_BACKGROUND_SUFFIX;
	}
	public final static String getItemUseCustomBackgroundKey (String item) {
		return item + EDITOR_CUSTOM_BACKGROUND_SUFFIX;
	}
	public final static String getItemBoldKey (String item) {
		return item + EDITOR_BOLD_SUFFIX;
	}
	
	public static final void setDefault(IPreferenceStore store, String item, RGB foreground, RGB background, boolean useCustomBackground, boolean bold) {
		if (store == null || item == null || item.length() == 0) return;
		if (foreground != null) 
			PreferenceConverter.setDefault(store, getItemForegroundKey(item), foreground);
		if (background != null)
			PreferenceConverter.setDefault(store, getItemBackgroundKey(item), background);
		store.setDefault(getItemUseCustomBackgroundKey(item), useCustomBackground);
		store.setDefault(getItemBoldKey(item), bold);
	}
	
	public final static String EDITOR_TAB_WIDTH = "org.jboss.tools.common.text.xml.ui.tab.width";
	
	public final static String EDITOR_REPLACE_TAB_WITH_WHITESPACE = "org.jboss.tools.common.text.xml.ui.tab.replace";


}
