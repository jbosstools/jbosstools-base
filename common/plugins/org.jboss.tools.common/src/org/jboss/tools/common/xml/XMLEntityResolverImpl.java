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
package org.jboss.tools.common.xml;

import java.io.IOException;
import java.io.InputStream;

import org.apache.xerces.xni.XMLResourceIdentifier;
import org.apache.xerces.xni.XNIException;
import org.apache.xerces.xni.parser.XMLEntityResolver;
import org.apache.xerces.xni.parser.XMLInputSource;

import org.eclipse.wst.wsdl.validation.internal.resolver.URIResolver;
import org.jboss.tools.common.CommonPlugin;

/**
 * @author Igels
 */
public class XMLEntityResolverImpl implements XMLEntityResolver {

    private URIResolver uriResolver;

    public XMLEntityResolverImpl() {
    }

    public XMLEntityResolverImpl(URIResolver uriResolver) {
        this.uriResolver = uriResolver;
    }

    public XMLInputSource resolveEntity(XMLResourceIdentifier rid) throws XNIException, IOException {
        XMLInputSource result = null;
        String systemId = null;
        String publicId = null;
        try {
            DtdResolver resolver = new DtdResolver();
            systemId = rid.getBaseSystemId()==null?rid.getLiteralSystemId():rid.getExpandedSystemId();
            publicId = rid.getPublicId();
        	
        	if(systemId != null && systemId.indexOf("www.ibm.com") >= 0) {
//        		CommonPlugin.getPluginLog().logError( "ignore");
//        		return null;
        	}
        	
            InputStream is = resolver.getInputStream(rid.getPublicId(), systemId);
            if(is!=null) {
                result = new XMLInputSource(rid.getPublicId(), systemId, rid.getBaseSystemId(), is, null);
            }
        } catch (Exception e) {
        	CommonPlugin.getPluginLog().logError( "Exception publicId=" + publicId + " systemId=" + systemId + " exception=" + e.getClass().getName() + ":" + e.getMessage());
        }

        return result;
    }
}