/******************************************************************************* 
 * Copyright (c) 2016-2017 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.reddeer.requirements;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.eclipse.reddeer.eclipse.jdt.ui.preferences.FoldingPreferencePage;
import org.eclipse.reddeer.junit.requirement.Requirement;
import org.eclipse.reddeer.workbench.ui.dialogs.WorkbenchPreferenceDialog;
import org.jboss.tools.common.reddeer.requirements.JavaFoldingRequirement.JavaFolding;

/**
 * @author Jeff Maury
 *
 */
public class JavaFoldingRequirement implements Requirement<JavaFolding> {
    private JavaFolding declaration;
    private Boolean javaFoldingOriginalValue;

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.TYPE)
    public @interface JavaFolding {

        /**
         * Value.
         *
         * @return true if the Java folding is on, false otherwise
         */
        boolean value() default true;
    }

    @Override
    public void fulfill() {
        WorkbenchPreferenceDialog dialog = new WorkbenchPreferenceDialog();
        dialog.open();
        FoldingPreferencePage pPage = new FoldingPreferencePage(dialog);
        dialog.select(pPage);
        javaFoldingOriginalValue = pPage.isFoldingChecked();
        if (javaFoldingOriginalValue != declaration.value()) {
        	if(declaration.value()) {
        		pPage.enableFolding();
        	} else {
        		pPage.disableFolding();
        	}
            dialog.ok();
        } else {
            dialog.cancel();
        }

    }

    @Override
    public void setDeclaration(JavaFolding declaration) {
        this.declaration = declaration;
    }

    @Override
    public void cleanUp() {
        if (javaFoldingOriginalValue != null) {
            WorkbenchPreferenceDialog dialog = new WorkbenchPreferenceDialog();
            dialog.open();
            FoldingPreferencePage pPage = new FoldingPreferencePage(dialog);
            dialog.select(pPage);
            if(javaFoldingOriginalValue) {
            	pPage.enableFolding();
            } else {
            	pPage.disableFolding();
            }
            dialog.ok();
        }
    }

	@Override
	public JavaFolding getDeclaration() {
		return declaration;
	}
}
