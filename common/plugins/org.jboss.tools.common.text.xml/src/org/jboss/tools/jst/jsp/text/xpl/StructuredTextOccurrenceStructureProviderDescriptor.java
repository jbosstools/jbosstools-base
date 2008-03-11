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
package org.jboss.tools.jst.jsp.text.xpl;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.jface.text.Assert;


/**
 * @author Jeremy
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * Describes a contribution to the folding provider extension point.
 * 
 * @since 3.0
 */
public final class StructuredTextOccurrenceStructureProviderDescriptor {

	/* extension point attribute names */
	
	private static final String PREFERENCES_CLASS= "preferencesClass"; //$NON-NLS-1$
	private static final String CLASS= "class"; //$NON-NLS-1$
	private static final String NAME= "name"; //$NON-NLS-1$
	private static final String EDITOR_ID= "editorId"; //$NON-NLS-1$
	
	/** The identifier of the extension. */
	private String fEditorId;
	/** The name of the extension. */
	private String fName;
	/** The class name of the provided <code>IJavaFoldingStructureProvider</code>. */
	private String fClass;
	/**
	 * <code>true</code> if the extension specifies a custom
	 * <code>IJavaFoldingPreferenceBlock</code>.
	 */
	private boolean fHasPreferences;
	/** The configuration element of this extension. */
	private IConfigurationElement fElement;
	
	/**
	 * Creates a new descriptor.
	 * 
	 * @param element the configuration element to read
	 */
	StructuredTextOccurrenceStructureProviderDescriptor(IConfigurationElement element) {
		fElement= element;
		fEditorId= element.getAttributeAsIs(EDITOR_ID);
		Assert.isLegal(fEditorId != null);
		
		fName= element.getAttribute(NAME);
		if (fName == null)
			fName= fEditorId;
		
		fClass= element.getAttributeAsIs(CLASS);
		Assert.isLegal(fClass != null);
		
		if (element.getAttributeAsIs(PREFERENCES_CLASS) == null)
			fHasPreferences= false;
		else
			fHasPreferences= true;
	}
	
	/**
	 * Creates a folding provider as described in the extension's xml.
	 * 
	 * @return a new instance of the folding provider described by this
	 *         descriptor
	 * @throws CoreException if creation fails
	 */
	public IStructuredTextOccurrenceStructureProvider createProvider() throws CoreException {
		IStructuredTextOccurrenceStructureProvider prov= (IStructuredTextOccurrenceStructureProvider) fElement.createExecutableExtension(CLASS);
		return prov;
	}

	/**
	 * Returns the identifier of the described extension.
	 * 
	 * @return Returns the id
	 */
	public String getEditorId() {
		return fEditorId;
	}
	
	/**
	 * Returns the name of the described extension.
	 * 
	 * @return Returns the name
	 */
	public String getName() {
		return fName;
	}
}
