/*******************************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
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
package org.jboss.tools.jst.jsp.preferences.xpl;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;

/**
 * @author Jeremy, tau
 *
 */
public class PreferenceConstants {
	/**
	 * A named preference that controls whether occurrences are marked in the editor.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 *
	 * @since 3.0
	 */	
	public static final String EDITOR_MARK_OCCURRENCES= "markOccurrences"; //$NON-NLS-1$

	/**
	 * A named preference that controls whether occurrences are sticky in the editor.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 *
	 * @since 3.0
	 */	
	public static final String EDITOR_STICKY_OCCURRENCES= "stickyOccurrences"; //$NON-NLS-1$
	
	/**
	 * A named preference that controls whether node occurrences are marked.
	 * Only valid if {@link #EDITOR_MARK_OCCURRENCES} is <code>true</code>.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 * 
	 * @since 3.0
	 */
	public static final String EDITOR_MARK_NODE_OCCURRENCES= "markNodeOccurrences"; //$NON-NLS-1$

	/**
	 * A named preference that controls whether text node occurrences are marked.
	 * Only valid if {@link #EDITOR_MARK_OCCURRENCES} is <code>true</code>.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 * 
	 * @since 3.0
	 */	
    public static String EDITOR_MARK_TEXT_NODE_OCCURRENCES = "markTextNodeOccurrences";
	
	/**
	 * A named preference that controls whether attribute occurrences are marked.
	 * Only valid if {@link #EDITOR_MARK_OCCURRENCES} is <code>true</code>.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 * 
	 * @since 3.0
	 */
	public static final String EDITOR_MARK_ATTRIBUTE_OCCURRENCES= "markAttributeOccurrences"; //$NON-NLS-1$
	/**
	 * A named preference that controls whether attribute value occurrences are marked.
	 * Only valid if {@link #EDITOR_MARK_OCCURRENCES} is <code>true</code>.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 * 
	 * @since 3.0
	 */
	public static final String EDITOR_MARK_ATTRIBUTE_VALUE_OCCURRENCES= "markAttributeValueOccurrences"; //$NON-NLS-1$
	
	/**
	 * A named preference that controls whether folding is enabled in the Java editor.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 * 
	 * @since 3.0
	 */
	public static final String EDITOR_FOLDING_ENABLED= "editor_folding_enabled"; //$NON-NLS-1$
	
	/**
	 * A named preference that stores the configured folding provider.
	 * <p>
	 * Value is of type <code>String</code>.
	 * </p>
	 * 
	 * @since 3.0
	 */
	public static final String EDITOR_FOLDING_PROVIDER= "editor_folding_provider"; //$NON-NLS-1$
	
	/**
	 * A named preference that stores the value for XML elements folding for the default folding provider.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 * 
	 * @since 3.0
	 */
	public static final String EDITOR_FOLDING_XML_ELEMENTS= "editor_folding_default_xml_elements"; //$NON-NLS-1$

	
	/**
	 * Initializes the given preference store with the default values.
	 * 
	 * @param store the preference store to be initialized
	 * 
	 * @since 2.1
	 */
	public static void initializeDefaultValues(IPreferenceStore store) {
		// set the default values from AbstractDecoratedTextEditor
		AbstractDecoratedTextEditorPreferenceConstants.initializeDefaultValues(store);
		
		// mark occurrences
		store.setDefault(PreferenceConstants.EDITOR_MARK_OCCURRENCES, true); // tau 09.11.2004
		store.setDefault(PreferenceConstants.EDITOR_STICKY_OCCURRENCES, true);
		store.setDefault(PreferenceConstants.EDITOR_MARK_NODE_OCCURRENCES, true);
		store.setDefault(PreferenceConstants.EDITOR_MARK_TEXT_NODE_OCCURRENCES, true);		
		store.setDefault(PreferenceConstants.EDITOR_MARK_ATTRIBUTE_OCCURRENCES, true);
		store.setDefault(PreferenceConstants.EDITOR_MARK_ATTRIBUTE_VALUE_OCCURRENCES, true);

		// folding
		store.setDefault(PreferenceConstants.EDITOR_FOLDING_ENABLED, true);
		store.setDefault(PreferenceConstants.EDITOR_FOLDING_PROVIDER, "org.jboss.tools.common.text.xml.ui.text.defaultFoldingProvider"); //$NON-NLS-1$
//		store.setDefault(PreferenceConstants.EDITOR_FOLDING_JAVADOC, false);
//		store.setDefault(PreferenceConstants.EDITOR_FOLDING_INNERTYPES, true);
//		store.setDefault(PreferenceConstants.EDITOR_FOLDING_METHODS, false);
//		store.setDefault(PreferenceConstants.EDITOR_FOLDING_IMPORTS, true);

	}

}
