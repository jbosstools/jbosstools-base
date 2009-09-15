/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.options;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;

/**
 * @author eskimo
 */
public class Preference {
	private String option;
	private String attributeName;
	public static final String OPTIONS_PATH = "%Options%"; //$NON-NLS-1$
	public static String EDITOR_PATH = "%Options%/Editors"; //$NON-NLS-1$
	public static final Preference SHOW_NATURE_WARNING = new Preference(EDITOR_PATH, "natureWarning"); //$NON-NLS-1$
	
	protected Preference(String optionPath, String attributeName) {
		option = optionPath;	
		this.attributeName = attributeName;
	}
	
	public String getValue() {
		XModelObject obj = PreferenceModelUtilities.getPreferenceModel().getByPath(option);
		if(obj == null) {
			if(ModelPlugin.isDebugEnabled()) {
				ModelPlugin.getPluginLog().logInfo(option + " does not exist"); //$NON-NLS-1$
			}
			return ""; //$NON-NLS-1$
		} 
		XAttribute attribute = obj.getModelEntity().getAttribute(attributeName);
		if(attribute == null) {
			if(ModelPlugin.isDebugEnabled()) {
				ModelPlugin.getPluginLog().logInfo(attributeName + " in " + option + " does not exist"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			return ""; //$NON-NLS-1$
		} 
		return obj.getAttributeValue(attributeName);
	}
	
	public String getModelPath() {
		return option;	
	}
	
	public String getName()	{
		return attributeName;
	}
	
	public void setValue(String value) throws XModelException {
		XModelObject obj = PreferenceModelUtilities.getPreferenceModel().getByPath(option);
		if(obj == null) {
			if(ModelPlugin.isDebugEnabled()) {
				ModelPlugin.getPluginLog().logInfo(option + " does not exist"); //$NON-NLS-1$
			}
			return;
		} 
		obj.getModel().changeObjectAttribute(obj, attributeName, value);
		obj.getModel().saveOptions();
	}
	
}
