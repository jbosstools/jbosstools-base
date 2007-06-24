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

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class PreferenceModelUtilities {

	private static class PreferenceModelHolder {
		public static XModel preferenceModel;

		static {
			try {
				String f = ModelPlugin.getDefault().getStateLocation().toString();
				Properties p = new Properties();
				p.setProperty(XModelConstants.WORKSPACE, f);
				preferenceModel = createPreferenceModel(p);
				ServiceDialog d = createServiceDialog();
				if(d != null) {
					d.setModel(preferenceModel);
					preferenceModel.setService(d);
				}
			} catch (Exception e) {
				ModelPlugin.log(e);
			}
		}

		private static ServiceDialog createServiceDialog() {
			try {
				return (ServiceDialog)ModelFeatureFactory.getInstance().createFeatureInstance("org.jboss.tools.common.model.ui.wizards.one.ServiceDialogImpl");
			} catch (Exception e) {
				if(ModelPlugin.isDebugEnabled()) {
					ModelPlugin.log("Cannot create service dialog.");
				}
			}
			return null;
		}
		
	}
	
	public static XModel getPreferenceModel() {
		return PreferenceModelHolder.preferenceModel;
	}
	
	public static XModel createPreferenceModel(Properties p) {
		try {
			p.putAll(System.getProperties());
			p.setProperty("rootEntity", "OptionRoot");
			return XModelFactory.getModel(p);
		} catch (Exception t) {
			ModelPlugin.log("Error in creating preference model", t);
		}
		return null;
	}
	
	public static void initPreferenceValue(XModel initialModel, Preference preference)
	{
		String value = preference.getValue(); 
		if (value == null || "".equals(value))
		{
			XModelObject object = initialModel.getByPath(preference.getModelPath());
			if (object != null)
			{
				String newValue = object.getAttributeValue(preference.getName());
				if (newValue != null && !newValue.equals(value))
					preference.setValue(newValue);
			}
		}
	}
}
