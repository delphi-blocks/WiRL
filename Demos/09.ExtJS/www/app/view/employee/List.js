/**
 * This view is an example list of people.
 */
Ext.define('SecondDemo.view.employee.List', {
    extend: 'Ext.grid.Panel',
    xtype: 'mainlist',

    requires: [
        'SecondDemo.store.Employee',
        'SecondDemo.view.employee.ListController',
        'Ext.toolbar.Toolbar'
    ],

    controller: 'employee-list',

    title: 'Employee',

    layout: 'fit',

    plugins: {
        ptype: 'rowediting',
        autoCancel: false,
        clicksToEdit: 1
    },    

    dockedItems : {
        xtype: 'toolbar',
        dock: 'top',        
        buttonsAlign: 'right',
        items: [
            { text: 'Search', iconCls: 'x-fa fa-search', handler: 'onSearchClick' },
            { text: 'New', iconCls: 'x-fa fa-plus', handler: 'onNewClick' },
            //{ text: 'Edit', iconCls: 'x-fa fa-pencil' },
            { text: 'Delete', iconCls: 'x-fa fa-minus', handler: 'onDeleteClick' },
            { text: 'Save', iconCls: 'x-fa fa-floppy-o', handler: 'onSaveClick' },
            { text: 'Cancel', iconCls: 'x-fa fa-undo', handler: 'onCancelClick' },
            { text: 'Print', iconCls: 'x-fa fa-print', handler: 'onPrintClick' }
        ]
    },

    store: {
        type: 'employee'
    },

    columns: [
        { text: 'First name',  dataIndex: 'FIRST_NAME', flex: 1, editor: { xtype: 'textfield', allowBlank: false } },
        { text: 'Last name', dataIndex: 'LAST_NAME', flex: 1, editor: { xtype: 'textfield', allowBlank: false } },
        { text: 'Phone ext.', dataIndex: 'PHONE_EXT', flex: 1, editor: 'numberfield' },
        { text: 'Hire date', dataIndex: 'HIRE_DATE', flex: 1, xtype:'datecolumn', format:'d/m/Y', editor: { xtype: 'datefield', allowBlank: false } },
        { text: 'Salary', dataIndex: 'SALARY', flex: 1, xtype: 'numbercolumn', format:'0.00', align: 'right' }
    ]/*,

    listeners: {
        select: 'onItemSelected'
    }*/
});
