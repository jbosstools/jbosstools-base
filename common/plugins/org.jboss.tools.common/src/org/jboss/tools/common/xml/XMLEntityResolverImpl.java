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

import javax.xml.transform.URIResolver;

import org.apache.xerces.xni.XMLResourceIdentifier;
import org.apache.xerces.xni.parser.XMLEntityResolver;
import org.apache.xerces.xni.parser.XMLInputSource;

import org.jboss.tools.common.CommonPlugin;

import org.xml.sax.SAXException;

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

    public XMLInputSource resolveEntity(XMLResourceIdentifier rid) throws IOException {
        XMLInputSource result = null;
        String systemId = null;
        String publicId = null;
        InputStream is = null;
        try {
            DtdResolver resolver = new DtdResolver();
            systemId = rid.getBaseSystemId()==null?rid.getLiteralSystemId():rid.getExpandedSystemId();
            publicId = rid.getPublicId();
        	
            is = resolver.getInputStream(rid.getPublicId(), systemId);
            if(is!=null) {
                result = new XMLInputSource(rid.getPublicId(), systemId, rid.getBaseSystemId(), is, null);
            }
        } catch (SAXException e) {
        	CommonPlugin.getPluginLog().logError( "Exception publicId=" + publicId + " systemId=" + systemId + " exception=" + e.getClass().getName() + ":" + e.getMessage()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        } 

        return result;
    }
}