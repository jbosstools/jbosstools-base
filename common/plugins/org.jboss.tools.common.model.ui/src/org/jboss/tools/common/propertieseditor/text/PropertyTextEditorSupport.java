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
package org.jboss.tools.common.propertieseditor.text;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.texteditors.TextEditorSupport;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.loaders.impl.PropertiesLoader;

/**
 * @author Jeremy
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class PropertyTextEditorSupport extends TextEditorSupport {
	PropertiesLoader loader = new PropertiesLoader();
	
	protected String loadContent() {
		XModelObject o = getModelObject();
		return (o == null ? "" : loader.getBody(o));
	}

	public void save() {
		if(lock > 0 || !isModified()) return;		
		lock++;
		try {
			loader.edit(getModelObject(), provider.getText());
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} finally {
			lock--;
			setModified (false);
		}
	}

}
