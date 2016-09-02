Ext.define('SecondDemo.store.Employee', {
    extend: 'Ext.data.Store',

    alias: 'store.employee',

    requires: [
        'SecondDemo.model.Employee'
    ],

    model: 'SecondDemo.model.Employee',

    autoLoad: true
});
