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
package org.jboss.tools.common.model.ui.editor;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.content.ITextContentDescriber;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.util.FileUtil;

/**
 * @author Viacheslav Kabanovich
 */
public class XMLContentDescriber extends org.eclipse.core.internal.content.XMLContentDescriber implements ITextContentDescriber {

	public int describe(InputStream contents, IContentDescription description) throws IOException {
		super.describe(contents, description);
		contents.reset();
		String text = FileUtil.readStream(contents);
		return describe(text, description);
	}
	
	public int describe(Reader contents, IContentDescription description) throws IOException {
		super.describe(contents, description);
		contents.reset();
		String text = read(contents);
		return describe(text, description);
	}
	
	private int describe(String text, IContentDescription description) {
		String entity = PreferenceModelUtilities.getPreferenceModel().getEntityRecognizer().getEntityName("xml", text);
		if(entity == null || entity.length() == 0 || entity.equals("FileXML") 
				|| entity.equals("FileHibConfig3") || entity.equals("FileHibernate3")
				|| entity.equals("FileANT")
			) {
			return INDETERMINATE;
		}
		return VALID;
	}

    static String read(Reader is) {
        StringBuffer sb = new StringBuffer("");
        try {
            char[] b = new char[4096];
            while(true) {
                int l = is.read(b, 0, b.length);
                if(l < 0) break;
                sb.append(new String(b, 0, l));
            }
            is.close();
        } catch (IOException e) {
        	CommonPlugin.getPluginLog().logError(e);
        }
        return sb.toString();
    }

}
