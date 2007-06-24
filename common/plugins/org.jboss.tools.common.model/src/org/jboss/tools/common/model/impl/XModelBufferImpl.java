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
package org.jboss.tools.common.model.impl;

import java.util.*;
import org.jboss.tools.common.model.*;

public class XModelBufferImpl implements XModelBuffer {
    private ArrayList<Pair> items = new ArrayList<Pair>();
    
    public XModelBufferImpl() {}

    public int getSize() {
        return items.size();
    }

    public XModelObject source() {
        return source(0);
    }

    public XModelObject copy() {
        return copy(0);
    }

    Pair getItem(int i) {
        return (getSize() <= i) ? null : (Pair)items.get(i);
    }

    public XModelObject source(int i) {
        Pair p = getItem(i);
        return (p == null) ? null : p.source;
    }

    public XModelObject copy(int i) {
        Pair p = getItem(i);
        return (p == null) ? null : p.copy();
    }

    public void setSource(XModelObject source) {
        clear();
        addSource(source);
    }

    public void addSource(XModelObject source) {
        items.add(new Pair(source));
    }

    public void clear() {
        items.clear();
    }

    class Pair {
        XModelObject source = null;
        XModelObject copy = null;
        public Pair(XModelObject source) {
            this.source = source;
        }
        public XModelObject copy() {
            if(copy == null && source != null) copy = source.copy();
            return copy;
        }
    }

}

