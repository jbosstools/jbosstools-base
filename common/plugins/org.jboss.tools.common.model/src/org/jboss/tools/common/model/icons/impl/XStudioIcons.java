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
package org.jboss.tools.common.model.icons.impl;

import java.io.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class XStudioIcons implements ImageComponent {
    private static final String code = "abcdefghijklmnop";

    public XStudioIcons() {}

    public String getImageString(String filename) {
        byte[] b = getBytes(filename);
        return (b == null) ? "" : encode(b);
    }

    public String encode(byte[] b) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < b.length; i++) {
            int h = 128 + b[i];
          sb.append(code.charAt(h / 16));
          sb.append(code.charAt(h % 16));
        }
        return sb.toString();
    }

    public byte[] decode(String s) {
        byte[] b = new byte[2*s.length()];
        int i = 0, k = 0;
        while(i < s.length()) {
            char c1 = s.charAt(i), c2 = s.charAt(i + 1);
            b[k] = (byte)(16 * code.indexOf(c1) + code.indexOf(c2) - 128);
            i += 2;
            ++k;
        }
        return b;
    }

    private byte[] getBytes(String filename) {
        File f = new File(filename);
        byte[] b = null;
        try {
            FileInputStream fr = new FileInputStream(f);
            int length = fr.available();
            b = new byte[length];
            int i = 0;
            while(i < length) {
                i += fr.read(b, i, length - i);
            }
        } catch (Exception e) {
            b = new byte[0];
        }
        return b;
    }

    public int getHash(XModelObject obj) {
        String s = obj.getAttributeValue("image");
        return (s == null || s.trim().length() == 0) ? "defaultimage".hashCode() : s.hashCode();
    }

    public Image getImage(XModelObject obj) {
        String s = obj.getAttributeValue("image");
        byte[] b = decode(s);
        if(b != null) {
        	try { 
        		ByteArrayInputStream is = new ByteArrayInputStream(b);
        		ImageData id = new ImageData(is);
        		Image i = new Image(null, id);
        		return i;
        	} catch (Exception e) {
        		ModelPlugin.getPluginLog().logError(e);
        	}
        }
   		return obj.getModelEntity().getMetaModel().getIconList().getImage("default.unknown");
    }

}
