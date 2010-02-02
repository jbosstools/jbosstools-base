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
package org.jboss.tools.common.ant.model;

import java.io.IOException;
import java.io.StringReader;

import org.eclipse.ant.internal.core.contentDescriber.AntBuildfileContentDescriber;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.content.IContentType;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class AntEntityRecognizer implements EntityRecognizer {
	private static final QualifiedName[] SUPPORTED_OPTIONS = new QualifiedName[] {IContentDescription.CHARSET, IContentDescription.BYTE_ORDER_MARK};
    public AntEntityRecognizer() {}
   
    public String getEntityName(EntityRecognizerContext context) {
    	return getEntityName(context.getExtension(), context.getBody());
    }

    String getEntityName(String ext, String body) {
        if(body == null) return null;
        IContentType cd = Platform.getContentTypeManager().getContentType("org.eclipse.ant.core.antBuildFile"); //$NON-NLS-1$
        AntBuildfileContentDescriber d = new AntBuildfileContentDescriber();
        int i = -1;
        try {
        	i = d.describe(new StringReader(body), null);
        	if(i == 2) {
        		IContentDescription aa = cd.getDescriptionFor(new StringReader(body), SUPPORTED_OPTIONS);
        	}
        } catch (IOException e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
        return i == 2 ? "FileANT" : null; //$NON-NLS-1$
        
//        AntParser p = new AntParser(body);
//        return (p.getTargets() != null) ? "FileANT" : null;
    }

}
