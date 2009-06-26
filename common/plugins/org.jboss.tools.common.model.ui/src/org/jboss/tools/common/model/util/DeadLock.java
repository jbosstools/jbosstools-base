package org.jboss.tools.common.model.util;

public class DeadLock {
    static class Friend {
        private final String name;
        public Friend(String name) {
            this.name = name;
        }
        public String getName() {
            return this.name;
        }
        public synchronized void bow(Friend bower) {
            System.out.format("%s: %s has bowed to me!%n",  //$NON-NLS-1$
                    this.name, bower.getName());
            
            bower.bowBack(this);
        }
        public synchronized void bowBack(Friend bower) {
            System.out.format("%s: %s has bowed back to me!%n", //$NON-NLS-1$
                    this.name, bower.getName());
        }        
    }
    
    public static void main(String[] args) {
        final Friend alphonse = new Friend("Alphonse"); //$NON-NLS-1$
        final Friend gaston = new Friend("Gaston"); //$NON-NLS-1$
        new Thread(new Runnable() {
            public void run() { alphonse.bow(gaston); }
        }).start();
        new Thread(new Runnable() {
            public void run() { gaston.bow(alphonse); }
        }).start();
    }
}
