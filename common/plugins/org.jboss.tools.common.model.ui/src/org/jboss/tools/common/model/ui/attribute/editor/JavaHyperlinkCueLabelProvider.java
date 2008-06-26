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
package org.jboss.tools.common.model.ui.attribute.editor;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.common.model.ui.ModelUIImages;

public class JavaHyperlinkCueLabelProvider extends LabelProvider {
	private static final Image CLASS_IMAGE = createClassImage();
	
	public interface JavaClassHolder {
		public boolean classExists();
	}
	
	static Image createClassImage() {
		try {
			return ModelUIImages.getImage("java/wizard.gif");
		} catch (Exception e) {
			return null;
		}
	}

	public static JavaHyperlinkCueLabelProvider INSTANCE = new JavaHyperlinkCueLabelProvider();
	
	private JavaHyperlinkCueLabelProvider() {}
	
	public String getText(Object element) {
		return null;
	}
	public Image getImage(Object element) {
		if(!(element instanceof Text)) return null;
		Text text = (Text)element;
		if(text.isDisposed()) return null;
		JavaClassHolder editor = (JavaClassHolder)text.getData("JavaHyperlinkLineFieldEditor");
	    return (editor == null || editor.classExists()) ? null : CLASS_IMAGE;
	}
	
}
