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

/**
 * @author Jeremy
 *
 */
public interface OccurrencePreferenceConstants {
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
	 * A named preference that stores the configured folding provider.
	 * <p>
	 * Value is of type <code>String</code>.
	 * </p>
	 * 
	 * @since 3.0
	 */
	public static final String EDITOR_OCCURRENCE_PROVIDER= "editor_occurrence_provider"; //$NON-NLS-1$

}
