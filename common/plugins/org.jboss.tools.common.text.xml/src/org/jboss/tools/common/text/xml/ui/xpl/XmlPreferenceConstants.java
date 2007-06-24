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

import java.lang.reflect.Method;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * @author Jeremy
 *
 */
public class XmlPreferenceConstants extends BasePreferenceConstants {

	private XmlPreferenceConstants() {
	}


	/**
	 * A named preference that controls whether bracket matching highlighting is turned on or off.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public final static String EDITOR_MATCHING_BRACKETS= "matchingBrackets"; //$NON-NLS-1$

	/**
	 * A named preference that holds the color used to highlight matching brackets.
	 * <p>
	 * Value is of type <code>String</code>. A RGB color value encoded as a string 
	 * using class <code>PreferenceConverter</code>
	 * </p>
	 * 
	 * @see org.eclipse.jface.resource.StringConverter
	 * @see org.eclipse.jface.preference.PreferenceConverter
	 */
	public final static String EDITOR_MATCHING_BRACKETS_COLOR=  "matchingBracketsColor"; //$NON-NLS-1$

	/**
	 * A named preference that controls whether the current line highlighting is turned on or off.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public final static String EDITOR_CURRENT_LINE= "currentLine"; //$NON-NLS-1$

	/**
	 * A named preference that holds the color used to highlight the current line.
	 * <p>
	 * Value is of type <code>String</code>. A RGB color value encoded as a string
	 * using class <code>PreferenceConverter</code>
	 * </p>
	 * 
	 * @see org.eclipse.jface.resource.StringConverter
	 * @see org.eclipse.jface.preference.PreferenceConverter
	 */
	public final static String EDITOR_CURRENT_LINE_COLOR= "currentLineColor"; //$NON-NLS-1$

	/**
	 * A named preference that controls whether the print margin is turned on or off.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public final static String EDITOR_PRINT_MARGIN= "printMargin"; //$NON-NLS-1$
	
	/**
	 * A named preference that holds the color used to render the print margin.
	 * <p>
	 * Value is of type <code>String</code>. A RGB color value encoded as a string
	 * using class <code>PreferenceConverter</code>
	 * </p>
	 * 
	 * @see org.eclipse.jface.resource.StringConverter
	 * @see org.eclipse.jface.preference.PreferenceConverter
	 */
	public final static String EDITOR_PRINT_MARGIN_COLOR= "printMarginColor"; //$NON-NLS-1$

	/**
	 * Print margin column. Int value.
	 */
	public final static String EDITOR_PRINT_MARGIN_COLUMN= "printMarginColumn"; //$NON-NLS-1$

	/**
	 * A named preference that controls if the line number ruler is shown in the UI.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 */
	public final static String EDITOR_LINE_NUMBER_RULER= "lineNumberRuler"; //$NON-NLS-1$

	/**
	 * A named preference that holds the color used to render line numbers inside the line number ruler.
	 * <p>
	 * Value is of type <code>String</code>. A RGB color value encoded as a string
	 * using class <code>PreferenceConverter</code>
	 * </p>
	 * 
	 * @see org.eclipse.jface.resource.StringConverter
	 * @see org.eclipse.jface.preference.PreferenceConverter
	 * @see #EDITOR_LINE_NUMBER_RULER
	 */
	public final static String EDITOR_LINE_NUMBER_RULER_COLOR= "lineNumberColor"; //$NON-NLS-1$

	/**
	 * Initializes the given preference store with the default values.
	 *
	 * @param store the preference store to be initialized
	 *
	 * @since 2.1
	 */
	public static void initializeDefaultValues(final IPreferenceStore store) {
		store.setDefault(XmlPreferenceConstants.EDITOR_MATCHING_BRACKETS, false);
		PreferenceConverter.setDefault(store, XmlPreferenceConstants.EDITOR_MATCHING_BRACKETS_COLOR, new RGB(192, 192,192));

		store.setDefault(XmlPreferenceConstants.EDITOR_CURRENT_LINE, true);
		PreferenceConverter.setDefault(store, XmlPreferenceConstants.EDITOR_CURRENT_LINE_COLOR, new RGB(225, 235, 224));

		store.setDefault(XmlPreferenceConstants.EDITOR_PRINT_MARGIN, false);
		store.setDefault(XmlPreferenceConstants.EDITOR_PRINT_MARGIN_COLUMN, 80);
		PreferenceConverter.setDefault(store, XmlPreferenceConstants.EDITOR_PRINT_MARGIN_COLOR, new RGB(176, 180 , 185));
		
		store.setDefault(XmlPreferenceConstants.EDITOR_LINE_NUMBER_RULER, false);
		PreferenceConverter.setDefault(store, XmlPreferenceConstants.EDITOR_LINE_NUMBER_RULER_COLOR, new RGB(0, 0, 0));
		
        store.setDefault(BasePreferenceConstants.EDITOR_TAB_WIDTH, 4);
        store.setDefault(BasePreferenceConstants.EDITOR_REPLACE_TAB_WITH_WHITESPACE, false);

		setDefault(store, "errorIndicationColor", new RGB(255,175,175), new RGB(255, 255, 255), false, false);
		
		initializeTextEditorValues(store);
	}
	
	static String[] BOOLEAN_PROPERTIES = new String[]{
		EDITOR_CURRENT_LINE, EDITOR_LINE_NUMBER_RULER, EDITOR_PRINT_MARGIN, "overviewRuler"
	};
	static String[] STRING_PROPERTIES = new String[]{
		EDITOR_CURRENT_LINE_COLOR, EDITOR_LINE_NUMBER_RULER_COLOR, EDITOR_PRINT_MARGIN_COLOR,
		"errorIndicationColor", "warningIndicationColor", "infoIndicationColor", "taskIndicationColor", "bookmarkIndicationColor"
	};
	static String[] INT_PROPERTIES = new String[]{EDITOR_PRINT_MARGIN_COLUMN};
	
	private static void initializeTextEditorValues(final IPreferenceStore store) {
		Plugin plugin = Platform.getPlugin("org.eclipse.ui.editors");
		try {
			Method m = AbstractUIPlugin.class.getDeclaredMethod("initializeDefaultPluginPreferences", new Class[0]);
			m.setAccessible(true);
			m.invoke(plugin, new Object[0]);
		} catch (Exception e) {}
		final IPreferenceStore editorsStore = ((AbstractUIPlugin)plugin).getPreferenceStore();
		for (int i = 0; i < BOOLEAN_PROPERTIES.length; i++) {
			String p = BOOLEAN_PROPERTIES[i];
			store.setValue(p, editorsStore.getBoolean(p));
		}
		for (int i = 0; i < STRING_PROPERTIES.length; i++) {
			String p = STRING_PROPERTIES[i];
			store.setValue(p, editorsStore.getString(p));
		}
		for (int i = 0; i < INT_PROPERTIES.length; i++) {
			String p = INT_PROPERTIES[i];
			store.setValue(p, editorsStore.getInt(p));
		}
		
		// these annotation preferences have a different default value than the one the base provides
		store.setDefault("errorIndicationInVerticalRuler", true); //$NON-NLS-1$
		store.setDefault("warningIndicationInVerticalRuler", true); //$NON-NLS-1$
		
		editorsStore.addPropertyChangeListener(new IPropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent event) {
				for (int i = 0; i < BOOLEAN_PROPERTIES.length; i++) {
					String p = BOOLEAN_PROPERTIES[i];
					if(!event.getProperty().equals(p)) continue;
					store.setValue(p, editorsStore.getBoolean(p));
					return;
				}
				for (int i = 0; i < STRING_PROPERTIES.length; i++) {
					String p = STRING_PROPERTIES[i];
					if(!event.getProperty().equals(p)) continue;
					store.setValue(p, editorsStore.getString(p));
					return;
				}
				for (int i = 0; i < INT_PROPERTIES.length; i++) {
					String p = INT_PROPERTIES[i];
					if(!event.getProperty().equals(p)) continue;
					store.setValue(p, editorsStore.getInt(p));
					return;
				}
			}
		});
	}
	
}
